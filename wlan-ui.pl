#!/usr/bin/perl -w 
# wlan-ui.pl: GUI script to get wireless access points, connect etc
#
# Based on wlan-zenity:
## Name:    WLAN-Connect 0.3               ##
## Author:  Mirza Muharemagic              ##
## Email:   mirza@php.co.ba                ##
## WWW:     http://www.php.co.ba/X31       ##
## License: BSD                            ##
#
# with many thanks.
# 
# This version by Matthew Brett (matthewb berkeley.edu)
#
# $Id: wlan-ui.pl,v 1.6 2005/01/06 04:12:30 matthewbrett Exp $ 

use Gtk2;
use Gtk2::GladeXML;
use Gtk2::SimpleList;

use IO::Select;

use Digest::MD5;

use File::Basename;
use Getopt::Long;
use Getopt::ArgvFile;
use Pod::Usage;

use strict;
use vars qw($VERSION);

$VERSION = 0.03;

# We need to know if the wireless interface is up when we start.  To
# do this, we check if the right module for the wireless interface is
# loaded ($MODULE variable).  If $MODULE is not defined, the interface
# is assumed to be always up, and no attempt is made to check, unload
# etc.  If $MODULE is not defined, you would not need to define the
# commands (see below): lsmod; modprobe; load; unload.

# ------------------------------------------------------------------
# Make edits below to fit you configuration
# ------------------------------------------------------------------

# Wireless driver module to load
my $MODULE = 'ipw2200';

# Wireless network device - e.g. 'wlan0'.
# If not defined we use /proc/net/wireless to find the device
my $device = undef;

# Commands for manipulating wlan module etc
# We will find unspecified commands from the path
my $CMDS = {'lsmod',  '/sbin/lsmod', 
	'modprobe', '/sbin/modprobe',
	'load',     undef,       # modprobe used by default
	'unload',   undef,       # modprobe -r used by default
	'iwconfig', '/sbin/iwconfig',
	'iwlist',   '/sbin/iwlist',
	'ifconfig', '/sbin/ifconfig',
	'ps',       undef,
	'dhcpcd',   '/sbin/dhclient'};

# ------------------------------------------------------------------
# You wouldn't normally want to edit below here
# ------------------------------------------------------------------


# Getopt::long option definitions
my(@opt_table) = (
		  "help|h",        # print help message and exit
		  "man|doc",       # print documentation and exit
		  "verbose",       # more messages 
		  "quiet",         # fewer messages
		  "version",       # to show version number
		  "ui!",           # flag to use UI
		  "autoconnect!",  # whether to try autoconnect or no
		  "key=s%",        # ESSID=wep_key_value pairs
		  "passwd=s%",     # ESSID=password pairs
		  "autoconnect_to=s@", # ESSIDs to do auto connect for
		  "list=s@",       # put ESSID at top of list for connect
		  "rescan_interval", # interval (seconds) to rescan if nothing found
		  "rescan_count", # times to rescan if nothing found
		  );

# Factory (!) option defaults
my(%opt_defs) = (
		 'quiet',   0,
		 'verbose', 0,
		 'ui',      1, 
		 'autoconnect', 0,
		 'rescan_interval', 2, 
		 'rescan_count', 1,
		 );

my($me, $my_path)  = fileparse($0, "");

# deal with option files
&Getopt::ArgvFile::argvFile(
			    default=> 1,
			    home=> 1,
			    current=> 1,
			    );

# get options
my(%OPTIONS) = ();
&GetOptions (\%OPTIONS, @opt_table) || exit 1;

# fill undefined options with defaults
my $key;
foreach $key(keys(%opt_defs)) {
    $OPTIONS{$key}=$opt_defs{$key} unless (defined($OPTIONS{$key}));
}
$OPTIONS{quiet} = 0 if $OPTIONS{verbose};

# help messages
printf "%s - version %4.2f\n", $me, $VERSION if ($OPTIONS{version});
pod2usage(-exitstatus => 0, -verbose => 2) if ($OPTIONS{man});
pod2usage(-exitstatus => 0, -verbose => $OPTIONS{verbose})
    if ($OPTIONS{help});

# exit if version number requested
exit 0 if ($OPTIONS{version});

# sort out commands
$CMDS = resolve_commands($MODULE, $CMDS);

# Load module if necessary
my $err;
die $err if ($err = wlan_mod('load', $MODULE, $CMDS));

# Get device name if not passed
$device = &get_wlan_device unless $device;

# Add device name to commands
my $cmd;
foreach $cmd(qw(iwlist iwconfig ifconfig dhcpcd)) {
    $CMDS->{$cmd} .= " $device";
}

# Clear old dhcp licences from this device
&clear_dhcp_clients($CMDS);

# Try autoconnect
my ($APS, $ap, $res);
$APS = &scan_order_aps($CMDS);
if ($OPTIONS{autoconnect}) {
    $ap = first_autoconnect($APS);
    if ($ap) {
	$res = no_ui_connect($ap, $CMDS);
	exit 0 if $res;
    } else {
	print STDERR "Could not find an AP to autoconnect to\n";
    }
}
exit 0 unless $OPTIONS{ui};

# Glade / Gtk stuff
Gtk2->init;
my $gladexml = Gtk2::GladeXML->new("$my_path/wlan-ui.glade");
$gladexml->signal_autoconnect_from_package('main');
my $W_MAIN = $gladexml->get_widget('w_main');
my $DLG_PASSWD = $gladexml->get_widget('dlg_passwd');
my $TE_PASSWD = $gladexml->get_widget('te_passwd');
my $RB_WEP = $gladexml->get_widget('rb_wep');
my $DLG_CONNECTED = $gladexml->get_widget('dlg_connected');

# Setup treeview lists
# Properties of connected AP
my $CNX_LIST =
    Gtk2::SimpleList->new_from_treeview(
					$gladexml->get_widget ('tv_connected'),
					'Parameter', 'text',
					'Value',     'text');

# List of connectable APs
my $SLIST =
    Gtk2::SimpleList->new_from_treeview(
					$gladexml->get_widget ('tv_aps'),
					'No'        => 'int',
					'ESSID'     => 'text',
					'MAC'       => 'text',
					'Mode'      => 'text',
					'Encr'      => 'text',
					'Quality'   => 'text',
					'Signal'    => 'text',
					'Noise'     => 'text',
					);

# Fill AP list
show_aps($APS);

# Launch UI; the rest are callbacks
Gtk2->main;

# Should not get here
exit 0;

# ---------------------------------------
# Subroutines 
# ---------------------------------------

# sort out and check options
sub resolve_commands {
    my ($module, $cmds) = @_;
    my $c;

    if ($module) { # We need commmands 
	$cmds = get_command($cmds, 'lsmod');
	unless ($cmds->{load} && $cmds->{unload}) {
	    $cmds = get_command($cmds, 'modprobe');
	}
	$cmds->{load} = "$cmds->{modprobe} $module"
	    unless $cmds->{load};
	$cmds->{unload} = "$cmds->{modprobe} -r $module" 
	    unless $cmds->{unload};
    }
    foreach $c(qw(iwconfig ifconfig iwlist dhcpcd ps)) {
	$cmds = get_command($cmds, $c);
    }
    return $cmds;
}

# check for, return commands, or die
sub get_command {
    my ($cmds, $cmd_name) = @_;
    unless ($cmds->{$cmd_name}) {
	die "Cannot find $cmd_name" 
	    unless chomp($cmds->{$cmd_name} = `which $cmd_name`);
    }
    return $cmds;
}

# load  / unload if needed; return undef or error string if error
sub wlan_mod {
    my ($cmd_key, $module, $cmds) = @_;
    my ($res);

    return undef unless $module;
    
    if (`$cmds->{lsmod}` !~ /$module/g) {
	$res = `$cmds->{$cmd_key}`;
	return "$res: could not $cmd_key $module" 
	    if (`$cmds->{lsmod}` !~ /$module/g); 
    }
    return undef;
}

# Add options to scanned APs
sub add_ap_options {
    my ($aps) = @_;
    my ($ap, $ac_id, $essid, $list_no, @list);
    @list = @{$OPTIONS{list}} if defined($OPTIONS{list});

    foreach $ap(@$aps) {
	$essid = $ap->{ESSID};
	foreach $ac_id(@{$OPTIONS{autoconnect_to}}) {
	    $ap->{autconnect_to} = 1 if ($essid eq $ac_id);
	}
	foreach $ac_id(keys(%{$OPTIONS{key}})) {
	    $ap->{key} = $OPTIONS{key}->{$ac_id} if ($essid eq $ac_id);
	}
	foreach $ac_id(keys(%{$OPTIONS{passwd}})) {
	    $ap->{passwd} = $OPTIONS{passwd}->{$ac_id} if ($essid eq $ac_id);
	}
	$ap->{listno} = @list;
	foreach $list_no(0..$#list) {
	    $ap->{listno} = $list_no if ($list[$list_no] eq $essid);
	}
    }
    return $aps;
}

# Returns scanned aps in correct order, and with options stuff set
sub scan_order_aps {
    my $cmds = shift;
    my ($aps, $scan_no);

    foreach $scan_no(1..$OPTIONS{rescan_count}) {
	$aps = scan_for_aps($cmds->{iwlist});
	last if (@$aps);
	sleep($OPTIONS{rescan_interval});
    }
    $aps = add_ap_options($aps);
    return order_aps($aps);
}
   
# Order processed list of access points according to list options
# Look first for specified access points, then strong signal
sub order_aps {
    my($aps) = shift;

    my @aps_arr = @$aps;

    @aps_arr = sort { my $val = $a->{listno} <=> $b->{listno}; 
		      $val ? $val :  $b->{Signal} <=> $a->{Signal}; } @aps_arr;

    return \@aps_arr;
}

# Scan for access points
sub scan_for_aps {
    my($command) = shift;

    my (@aps);

    my $apoints = `$command scan`;
    my @spoints = split /Cell/, $apoints;
    shift(@spoints);

    ## parse found APs
    for (my $i=0; $i<=$#spoints; $i++) {
	$aps[$i] = parse_ap_info($spoints[$i]);
    }
    return \@aps;
}    

# Parse ifconfig info into hash ref
sub parse_iwconfig {
    my ($iw_cmd) = shift;
    my $iw_string = `$iw_cmd`;

    return parse_ap_info($iw_string);
}
   
# Parse iwconfig or iwlist output to hash ref
sub parse_ap_info {
    my ($ap_str) = shift;
    my %ap_info;
    
    if ($ap_str =~/(Address|Access point):\s*(\w\w:\w\w:\w\w:\w\w:\w\w:\w\w)/i) {
	$ap_info{Address} = $2;
    }
    if ($ap_str =~/ESSID:\s*\"(.*?)\"/i) {
	$ap_info{ESSID} = $1;
    } else { $ap_info{ESSID} = 'any' };
    if ($ap_str =~/Mode:\s*(Managed|Ad\-hoc|Auto|Master)\s*/i) {
	$ap_info{Mode} = $1;
    }
    if ($ap_str =~/Frequency:\s*([\w\.]+)\s*/i) {
	$ap_info{Frequency} = $1;
    }
    if ($ap_str =~/Encryption key:\s*(on|off)\s*/i) {
	$ap_info{Encryption} = $1;
    }
    if ($ap_str =~/Quality:(.*?)\s/i) {
	$ap_info{Quality} = $1;
    }
    if ($ap_str =~/Signal level:([\-0-9.]*)/i) {
	$ap_info{Signal} = $1;
    }
    if ($ap_str =~/Noise level:([\-0-9.]*)/i) {
	$ap_info{Noise} = $1;
    }
    return \%ap_info;
}

# Parse ifconfig info into hash ref
sub parse_ifconfig {
    my ($if_cmd) = shift;
    my %if_info = ('ip',    '<no IP-Address>', 
		   'bcast', '<Unknown>',
		   'mask',  '<Unknown>',
		   );
    my $if_string = `$if_cmd`;

    if ($if_string =~/inet addr:(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})/gi) {
        $if_info{ip} = $1;
    } 
    if ($if_string =~/Bcast:(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})/gi) {
        $if_info{bcast} = $1;
    } 
    if ($if_string =~/Mask:(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})/gi) {
        $if_info{mask} = $1;
    } 
    return \%if_info;
}

# Returns first AP capable of autoconnect
sub first_autoconnect {
    my $aps = shift;
    my ($ap, $good_ap, $key);

    foreach $ap(@$aps) {
	if ($ap->{autconnect_to}) {
	    $key = ap_key($ap);
	    if ($key) {
		$ap->{key} = $key;
		$good_ap = $ap; 
		last;
	    }
	}
    }
    return $good_ap;
}

# get AP key
sub ap_key {
    my $ap = shift;
    return 'off' if ($ap->{Encryption} eq 'off');
    return $ap->{key} if ($ap->{key});
    return MakeWepKey($ap->{passwd}) if ($ap->{passwd});
    return undef;
}

# try to connect to specified AP, return config info if successful,
# undef otherwise
sub no_ui_connect {
    my ($ap, $cmds) = @_;
    &configure_device($ap, $cmds);
    system($cmds->{dhcpcd});
    return dhcp_result($cmds);
}

# configure wlan device using iwconfig
sub configure_device {
    my ($ap, $cmds) = @_;
    my $wep_key = $ap->{key} ? $ap->{key} : 'off';
    my $cmd = "$cmds->{iwconfig} " .
	"key $wep_key essid \"$ap->{ESSID}\"";
    print "Command $cmd\n" if $OPTIONS{verbose};
    my $res = `$cmd`;
}

# Return results from attempted connection
sub dhcp_result {
    my $cmds = shift;
    my ($iw_res, $ip_report, $key);

    $iw_res = parse_iwconfig($cmds->{iwconfig});
    $ip_report = parse_ifconfig($cmds->{ifconfig});

    foreach $key(keys(%$iw_res)) {
	$ip_report->{$key} = $iw_res->{$key};
    }

    return ($ip_report->{ip} eq "<no IP-Address>") ? undef : $ip_report;
}

# Wrapper to routine to find wlan device (to allow wait for device)
sub get_wlan_device {
    my $wlan_device;
    $wlan_device = &FindWLANdevice;
    unless ($wlan_device) {
	# Maybe still waiting after modprobe
	sleep(10);
	$wlan_device = &FindWLANdevice;
    }
    die "WLAN device not found. Please check your configuration" 
	unless $wlan_device;
    return $wlan_device;
}

## find wlan device (eth0 or eth1 etc)
# (function from wlan-zenity)
sub FindWLANdevice {
    my $WLANdevice;

    ## find device in proc
    open WLANDEV, '/proc/net/wireless';
    while (<WLANDEV>) {
        if ($_ =~/(eth|wlan)(\d)/) {
            $WLANdevice = $1.$2;
        }
    }
    close WLANDEV;
    return $WLANdevice;
}

## it converts password to wep key 
## copyright Aki Mimoto, see the link below
## http://mattfoster.clara.co.uk/madwifi-faq.htm
## (function from wlan-zenity)
sub MakeWepKey {
    return (substr Digest::MD5::md5_hex( substr( shift()  x 64, 0, 64 ) ), 
	    0, 26);
}

# Fills main window with AP information
sub show_aps {
    my($aps) = shift;

    my $ap;
    my $ctr = 1;
    @{$SLIST->{data}} = ();
    return unless $aps;
    for $ap(@$aps) {
	push @{$SLIST->{data}}, [ $ctr,
				  $ap->{ESSID},
				  $ap->{Address},
				  $ap->{Mode},
				  $ap->{Encryption},
				  $ap->{Quality},
				  $ap->{Signal},
				  $ap->{Noise}];
	$ctr++;
    }
}

# kills any dhcp clients for this device
sub clear_dhcp_clients {
    my $cmds = shift;
    my ($pid, $tmp, $cmd1, $cmd2);

    my($dhfile, $device) = split(' ', $cmds->{dhcpcd});
    $dhfile = fileparse($dhfile, "");
    
    open(PS_F, "$cmds->{ps} ax|");
    while (<PS_F>) {
	($pid,$tmp,$tmp,$tmp,$cmd1,$cmd2) = split;
	next unless ($cmd1 =~ /$dhfile/);
	next unless ($cmd2 =~ /$device/);
	kill 9, $pid;
    }
    close(PS_F);
}

# Connect to given AP showing various dialog boxes on the way
sub ui_connect {
    my ($ap, $cmds) = @_;
    my $tf = 0;
    my $ptf = 0;
    my $tm_out = 0.5;
    my ($cmd, $pid, $select_o, @ready, $line);

    # Do we need a password?
    my $key = ap_key($ap);
    unless ($key) {
	$ptf = ($DLG_PASSWD->run eq 'ok');
	$DLG_PASSWD->hide;
	return undef unless $ptf;
	$key = $TE_PASSWD->get_text();
	$key = MakeWepKey($key) unless $RB_WEP->get_active();
    }

    $ap->{key} = $key;
    &configure_device($ap, $cmds);

    my $dialog = Gtk2::Dialog->new ('Connecting...', $W_MAIN,
				    ['destroy-with-parent', 'modal'],
				    'gtk-cancel' => 'none');
    my $label = Gtk2::Label->new ('Connecting...');
    $dialog->vbox->add($label);
    
    $cmd = $cmds->{dhcpcd};
    die "Could not fork $cmd" unless $pid = open(CHILD1, "$cmd 2>&1 |");
    $dialog->signal_connect(response => sub { 
	kill 9, $pid; 
	waitpid $pid, 0;
	$_[0]->destroy; 
    } );

    $dialog->show_all;

    $select_o = IO::Select->new();
    $select_o->add(\*CHILD1);
    
    while($dialog->get_pointer != -1) {
	if (@ready = $select_o->can_read($tm_out)) {
	    if (eof(CHILD1)) {
		$tf = 1;
		$dialog->destroy;
		last;
	    }
	    $line = <CHILD1>;
	    $label->set_text("$line");
	}
	Gtk2->main_iteration while Gtk2->events_pending;
    }
    close(CHILD1);

    # If $tf not set, must have been cancelled
    return undef unless ($tf);

    my $ip_res = dhcp_result($cmds);

    # If $ip_res is undefined, connect failed
    unless ($ip_res) {
	$dialog = 
	    Gtk2::MessageDialog->new ($W_MAIN,
				      [qw/modal destroy-with-parent/],
				      'info',
				      'ok' ,
				      'Sorry - connection failed');
	$dialog->run;
	$dialog->destroy; 
	return undef;
    }

    @{$CNX_LIST->{data}} = ( ['IP address', $ip_res->{ip}     ],
			     ['Broadcast',  $ip_res->{bcast}  ],
			     ['Netmask',    $ip_res->{mask}   ],
			     ['ESSID',      $ip_res->{ESSID}  ],
			     ['AP MAC',     $ip_res->{Address}] );
    
    # Check for cancel
    unless ($DLG_CONNECTED->run eq 'ok') {
	$ip_res = undef;
	&clear_dhcp_clients($cmds);
    }

    $DLG_CONNECTED->hide;
    return $ip_res;
}

# Return first AP matching required ESSID
sub get_ap_by_essid {
    my ($aps, $essid) = @_;
    my $ap;
    foreach $ap(@$aps) {
	return $ap if ($ap->{ESSID} eq $essid);
    }
    return undef;
}

# -----------------------------------------------------
# Gtk+ callbacks & c
# -----------------------------------------------------

sub gtk_main_quit {
    Gtk2->main_quit;
    return 0;
}

sub on_w_main_delete_event {
    Gtk2->main_quit;
    return 0;
}

sub on_b_quit_unload_clicked {
    my $err;
    warn $err if ($err = wlan_mod('unload', $MODULE, $CMDS));
    Gtk2->main_quit;
    return 0;
}

sub on_b_rescan_clicked {
    $APS = &scan_order_aps($CMDS);
    show_aps($APS);
}

sub on_b_connect_clicked {
    my @sel = $SLIST->get_selected_indices;
    unless (@sel) {
	my  $dialog = 
	    Gtk2::MessageDialog->new ($W_MAIN,
				      [qw/modal destroy-with-parent/],
				      'info',
				      'ok' ,
				      'Please select an AP');
	$dialog->run;
	$dialog->destroy; 
	return;
    }
    my $essid = ${$SLIST->{data}}[$sel[0]][1];
    my $ap = get_ap_by_essid($APS, $essid);
    die "Cannot find AP $essid" unless $ap;
    
    Gtk2->main_quit if ( ui_connect($ap, $CMDS) );
}

1;

__END__

=head1 NAME

wlan-ui.pl - UI for selecting and connecting to WLAN access points (APs)

=head1 SYNOPSIS

wlan-ui.pl -key my_essid=1234567890 -list my_essid

Options:

    -help            print help message and quit
    -man             full documentation and quit
    -verbose         more messages, more detailed help output
    -quiet           no messages during operation
    -version         returns version no
    -key             ESSID=wep_key_value pairs
    -passwd          ESSID=password pairs
    -list            specify  ESSID to put at top of list for connect
    -rescan_interval interval (in seconds) to rescan if no APs found
    -rescan_count    number of times to rescan if no APs found
    -autoconnect     try to connect to any APs listed as good for autoconnect
    -autoconnect_to  give ESSID name of AP for which to try autoconnect 
    -noui            just do autoconnect, don't start UI for connect if fails 

    @configfile 

    Configuration file containing any of the options above in format
    given by Getopts::Argvfile (www.cpan.org) - the format is
    the same as for the command line, but allowing multiple
    lines and comments.
    

=head1 OPTIONS

=over 8

=item B<-help>

Print a long help message and exit.

=item B<-man>

Output the man page and exit.

=item B<-verbose>

Gives out some extra messages as it works.  Adding -verbose to -help
outputs more help.

=item B<-quiet>

Gives less messages. 

=item B<-version>

Returns version no to the console.

=item B<-key>

Specify WEP key(s) to use for connecting to a particular ESSID; e.g.

  wlan-ui.pl -key my_essid=1234567890 -key other_essid=0987654321

=item B<-passwd>

Specify password(s) to use for connecting to a particular ESSID; e.g.

 wlan-ui.pl -passwd my_essid=my_pass1 -key other_essid=my_pass2

=item B<-list>

Specifies one or more ESSIDs to put at top of list for connect; e.g.

 wlan-ui.pl -list my_essid -list other_essid

The APs are listed in the order they appear on the command line (or in
the options file - see below).  APs are only listed if they also
appear in the interface scan results.  APs not specified using the
-list option, but detected by the interface scan, are listed in order
of signal strength.

=item B<-rescan_interval>

Sets the interval (in seconds) to wait before rescanning for APs, if
no APs found (default 2).

=item B<-rescan_count>

Sets the number of times to rescan if no APs found (default 1).

=item B<-autoconnect>

If this option is set, wlan-ui.pl will search for any APs listed as
good for autoconnecting (see -autoconnect_to below), and which
either do not need encryption, or have passwords / WEP keys given
elsewhere in the options (see -key, -passwd).  If it finds such an AP,
it will try and connect.  If the connection is successful, the GUI
will not start, and the program stops.  If it fails, by default,
wlan-ui.pl launches the GUI (see -noui option below).

=item B<-autoconnect_to>

Give one or many ESSID names of APs for which to try autoconnect; e.g.

 wlan-ui.pl -autoconnect_to my_essid -autoconnect_to other_essid

=item B<-noui>

If this option is set, the program will only try and autoconnect; if
it fails, it will not launch the GUI to select another AP.

=back

=head1 CONFIGFILE

Options can be specified by configuration files in the same format as
for the command line, except options can be specified across many
lines, and comments can be interposed: e.g

 wlan-ui.pl -verbose @myconfig

where "myconfig" might be the following text file

 # Configuration file for wlan-ui perl program
 -key "my_essid"='1234567890'
 -key 'test essid'='0987654321'
 -list my_essid
 -list other_essid
 -autoconnect_to my_essid
 -autoconnect

This file sets the WEP keys for my_essid and test_essid, puts my_essid
and other_essid at the top of listed APs (if they turn up on a
scan), and will attempt to connect to my_essid on startup.

Configuration files are read in the following order: .wlan-ui.pl in the
directory of the wlan-ui.pl script; .wlan-ui.pl in your home directory;
.wlan-ui.pl in the current directory; and any configuration file passed
on the command line.  Options in files read later override those in
earlier files.  Options passed on the command line override options
passed in .wlan-ui.pl files, and any options specified earlier on the
the command line than the @configfile item.

=head1 REQUIREMENTS

You will need various CPAN modules installed E<lt>http://www.cpan.orgE<gt>:

   Gtk2
   Gtk2::GladeXML
   IO::Select
   Digest::MD5
   Getopt::ArgvFile
   Pod::Usage

=head1 INSTALLATION

Installation is simple and inelegant.  Copy the two distribution files
(wlan-ui.pl, wlan-ui.glade) to the same directory.  Edit the top of
the wlan-ui.pl file to match your system.  There are a small number of
relevant things you may want to edit.  Here is one example, using the
ndiswrapper module (http://ndiswrapper.sourceforge.net) as my wireless
driver, which gives me a network device attached to 'wlan0'.

  # Wireless driver module to load
  my $MODULE = 'ndiswrapper';

  # Wireless network device - e.g. 'wlan0'.
  # If not defined we use /proc/net/wireless to find the device
  my $device = 'wlan0';

  # Commands for manipulating wlan module etc
  # We will find unspecified commands from the path
  my $CMDS = {'lsmod',  '/sbin/lsmod', 
	    'modprobe', '/sbin/modprobe',
	    'load',     undef,       # modprobe used by default
	    'unload',   undef,       # modprobe -r used by default
	    'iwconfig', '/sbin/iwconfig',
	    'iwlist',   '/sbin/iwlist',
	    'ifconfig', '/sbin/ifconfig',
	    'ps',       undef,
	    'dhcpcd',   '/sbin/dhclient'};

The 'load' and 'unload' commands are usually left undefined, as above,
but if you want to use scripts, you can define them.  For example, if
there is a startup/shutdown script for the wireless connection that is
named after the wireless module, you could try something like:

  my $CMDS = {'lsmod',  '/sbin/lsmod', 
	    'modprobe', undef,      # not needed in this case
	    'load',     "/sbin/service $MODULE start",
	    'unload',   "/sbin/service $MODULE stop", 
  ...

If you leave the network device name ($device) undefined, the script
will try and find the device using the /proc/net/wireless interface.
If this fails, you will get messages including "WLAN device not
found", and will need to set your wireless device name in the $device
variable.

wlan-ui.pl will need root permissions to run the various wireless
configuration programs.  I do this by giving myself sudo permission to
run the script.  Here's some excerpts from my /etc/sudoers file:

   # Host alias specification
   Host_Alias   LAPTOP = fetastra

   # User alias specification
   User_Alias   WIRELESS = myself, myfriend

   # Run wlan-ui
   WIRELESS LAPTOP = NOPASSWD: /usr/local/bin/wlan-ui/wlan-ui.pl

I can then run wlan-ui.pl with

  sudo /usr/local/bin/wlan-ui.pl

For some security, make sure wlan-ui.pl is writeable only by root.

=head1 DESCRIPTION

wlan-ui.pl is a program to connect to wireless networks.  It can be
run as a GUI which will offer a list of available networks to connect
to.  

wlan-ui.pl is based on wlan-zenity, written by Mirza Muharemagic
(http://www.php.co.ba/X31) and released under the BSD license.

=head1 FEATURES

See description of options for list of features.

=head1 ODD FEATURES

We don't currently use the MAC address of the AP when connecting.
This will give odd effects when there is more than one AP with the
same ESSID.  Search for the get_ap_by_essid function in the wlan-ui.pl
file to see where this will cause a problem.

=head1 AUTHORS

Matthew Brett E<lt>matthewb berkeley.eduE<gt>

Mirza Muharemagic E<lt>http://www.php.co.ba/X31E<gt>

=head1 LICENSE

This program is free software, you can redistribute it and/or modify it
under the terms of the Artistic License distributed with Perl version
5.003 or (at your option) any later version. Please refer to the
Artistic License that came with your Perl distribution for more
details.

=cut
