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
# $Id: wlan-ui.pl,v 1.9 2005/04/01 21:55:11 matthewbrett Exp $ 

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

use vars qw($VERSION $MODULE $MODULEPARAMS $DEVICE $CMDS);

$VERSION = 0.04;

# ------------------------------------------------------------------
# You can make edits below to fit you configuration.  It's probably
# better to do this using a configuration file though; see the
# INSTALLATION section in the program help for more detail.  Note
# that, if you do edit below, then any settings will be overwritten by
# the configuration file, if present.
# ------------------------------------------------------------------

# Wireless driver module to load
$MODULE = 'ipw2200';

# Module parameters
$MODULEPARAMS = '';

# Wireless network device - e.g. 'wlan0'.
# If not defined we use /proc/net/wireless to find the device
$DEVICE = undef;

# Commands for manipulating wlan module etc
# We will find unspecified commands from the path
$CMDS = {'lsmod',  '/sbin/lsmod', 
	'modprobe', '/sbin/modprobe',
	'load',     undef,       # modprobe used by default
	'unload',   undef,       # modprobe -r used by default
	'iwconfig', '/sbin/iwconfig',
	'iwlist',   '/sbin/iwlist',
	'ifconfig', '/sbin/ifconfig',
	'ps',       undef,
	'dhcpcd',   '/sbin/dhclient'};

# Name of system configuration file 
my $sys_config = '/etc/wlan-uirc';

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
		  "quit_on_connect" # whether to quit interface when connected
		  "autoconnect!",  # whether to try autoconnect or no
		  "autoconnect_to=s@", # ESSIDs to do auto connect for
		  "key=s%",        # ESSID=wep_key_value pairs
		  "passwd=s%",     # ESSID=password pairs
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

# load system configuration file if present
do $sys_config if (-e $sys_config);

# help messages
printf "%s - version %4.2f\n", $me, $VERSION if ($OPTIONS{version});
pod2usage(-exitstatus => 0, -verbose => 2) if ($OPTIONS{man});
pod2usage(-exitstatus => 0, -verbose => $OPTIONS{verbose})
    if ($OPTIONS{help});

# exit if version number requested
exit 0 if ($OPTIONS{version});

# sort out commands
$CMDS = resolve_commands($MODULE, $MODULEPARAMS, $CMDS);

# Load module if necessary
my $err;
die $err if ($err = wlan_mod_load($MODULE, $CMDS));

# Get device name if not passed
$DEVICE = &get_wlan_device unless $DEVICE;

# Add device name to commands
my $cmd;
foreach $cmd(qw(iwlist iwconfig ifconfig dhcpcd)) {
    $CMDS->{$cmd} .= " $DEVICE";
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

# 'Load' glade-xml from DATA section
my $glade_data; {local $/ = undef; $glade_data = <DATA>;}

# Load the UI from xml definition
my $gladexml = Gtk2::GladeXML->new_from_buffer($glade_data);
$gladexml->signal_autoconnect_from_package('main');

# Set various global variables we will use in callbacks
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
    my ($module, $moduleparams, $cmds) = @_;
    my $c, $locale_str;

    if ($module) { # We need commmands 
	$cmds = get_command($cmds, 'lsmod');
	unless ($cmds->{load} && $cmds->{unload}) {
	    $cmds = get_command($cmds, 'modprobe');
	}
	$cmds->{load} = "$cmds->{modprobe} $module $moduleparams"
	    unless $cmds->{load}; 
	$cmds->{unload} = "$cmds->{modprobe} -r $module" 
	    unless $cmds->{unload};
    }
    foreach $c(qw(iwconfig ifconfig iwlist dhcpcd ps)) {
	$cmds = get_command($cmds, $c);
    }
    $locale_str = "LC_ALL=C";
    foreach $c(qw(iwconfig ifconfig)) {
	$cmds->{$c} = "$locale_str  ; $cmds->{$c}"
	    unless $cmds->{$c} ~= $locale_str;
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

# load  if needed; return undef or error string if error
sub wlan_mod_load {
    my ($module, $cmds) = @_;
    my ($res);

    return undef unless $module;
    
    if (`$cmds->{lsmod}` !~ /$module/g) {
	$res = `$cmds->{load}`;
	return "$res: could not load $module" 
	    if (`$cmds->{lsmod}` !~ /$module/g); 
    }
    return undef;
}

# unload if needed; return undef or error string if error
sub wlan_mod_unload {
    my ($module, $cmds) = @_;
    my ($res);

    return undef unless $module;
    
    if (`$cmds->{lsmod}` =~ /$module/g) {
	$res = `$cmds->{unload}`;
	return "$res: could not unload $module" 
	    if (`$cmds->{lsmod}` =~ /$module/g); 
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
    my $iw_string = ` $iw_cmd`;

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
    warn $err if ($err = wlan_mod_unload($MODULE, $CMDS));
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

    Gtk2->main_quit if ( ui_connect($ap, $CMDS) & $OPTIONS{quit_on_connect});
}

1;

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
    -quit_on_connect whether to quit program when successfully connected
    -autoconnect     try to connect to any APs listed as good for autoconnect
    -autoconnect_to  give ESSID name of AP for which to try autoconnect 
    -noui            just do autoconnect, don't start UI for connect if fails 
    @configfile 

    Options configuration file containing any of the options above in
    format given by Getopts::Argvfile E<lt>www.cpan.orgE<gt> - the
    format is the same as for the command line, but allowing multiple
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

=item B<-quit_on_connect>

By default, and with the -quit_on_connect option, the program will
finish when connection to an AP is successful.  To reverse this
default (so program does not quit) use the -no_quit_on_connect version
of this option.

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

wlan-ui.pl uses two types of configuration file, a system
configuration file (see INSTALLATION below), and option configuration
files, described here.

Option configuration files can specify options in the same format as
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

Installation is simple and inelegant.  Copy the program file
(wlan-ui.pl) to a directory on your path.  Next, create a new system
configuration file to reflect your system.  The system configuration
file is different from the options configuration file (@configfile,
above).  The system configuration file tells the program how to
configure the wireless interface, and the options configuration file
sets defaults for access points and other things.  The default
location of the system configuration file is /etc/wlan-uirc; you can
change this by editing the contents of the $sys_config variable in the
program file. Below is an example file, which uses the ipw2200 module
E<lt>http://ipw2100.sourceforge.netE<gt> as my wireless driver.  By
default this gives me a network device attached to 'wlan0'.

  # Example system configuration file 
  # Save as /etc/wlan-uirc

  # Wireless driver module to load
  $MODULE = 'ipw2200';

  # Module parameters
  $MODULEPARAMS = '';

  # Wireless network device - e.g. 'wlan0'.
  # If not defined we use /proc/net/wireless to find the device
  $DEVICE = undef;

  # Commands for manipulating wlan module etc
  # We will find unspecified commands from the path
  $CMDS = {'lsmod',  '/sbin/lsmod', 
	   'modprobe', '/sbin/modprobe',
	   'load',     undef,       # modprobe used by default
	   'unload',   undef,       # modprobe -r used by default
	   'iwconfig', '/sbin/iwconfig',
	   'iwlist',   '/sbin/iwlist',
	   'ifconfig', '/sbin/ifconfig',
	   'ps',       undef,
	   'dhcpcd',   '/sbin/dhclient'};

The 'load' and 'unload' commands are usually left undefined, as above,
but if you want to use scripts, you can define them.  For example, the
driverloader wireless driver E<lt>http://www.linuxant.comE<gt> uses a
system init script to start and stop the wireless driver.  To make
this work, you could try something like:

  $MODULE = 'driverloader';  
  $CMDS = {'lsmod',  '/sbin/lsmod', 
	   'modprobe', undef,      # not needed in this case
	   'load',     "/sbin/service $MODULE start",
	   'unload',   "/sbin/service $MODULE stop", 
  ...

Note the quotes (") around the load and unload command, to allow
variable substitution.

If you leave the $DEVICE variable undefined, the script will try and
find your wireless network device using the /proc/net/wireless
interface.  If this fails, you will get messages including "WLAN
device not found", and will need to set the $DEVICE variable to the
name of your wireless interface (e.g. $DEVICE = 'wlan0';).

You can leave $MODULE undefined, in which case the wireless interface
is assumed to be always up, and no attempt is made to check, unload
etc, and you don't need to define lsmod; modprobe; load; unload.

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

Note that the system configuration file (/etc/wlan-uirc) can contain
commands that will be executed from within the program.  You may want
to make sure that the configuration file is only writeable by root.

=head1 DESCRIPTION

wlan-ui.pl is a program to connect to wireless networks.  It can be
run as a GUI which will offer a list of available networks to connect
to.  

wlan-ui.pl is based on wlan-zenity, written by Mirza Muharemagic
E<lt>http://www.php.co.ba/X31E<gt> and released under the BSD license.

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

#----------------------------------------------------------------------
# We can append the glade file here instead of loading from file
#----------------------------------------------------------------------
# Loading from a file is just as easy; the file would merely be
# everything after and not including the __DATA__ line.  Such a file
# could be loaded with something like:
#
# my $gladexml = Gtk2::GladeXML->new("$my_path/wlan-ui.glade");

__DATA__
<?xml version="1.0" standalone="no"?> <!--*- mode: xml -*-->
<!DOCTYPE glade-interface SYSTEM "http://glade.gnome.org/glade-2.0.dtd">

<glade-interface>
<requires lib="gnome"/>

<widget class="GtkWindow" id="w_main">
  <property name="visible">True</property>
  <property name="title" translatable="yes">WLAN-UI</property>
  <property name="type">GTK_WINDOW_TOPLEVEL</property>
  <property name="window_position">GTK_WIN_POS_CENTER</property>
  <property name="modal">False</property>
  <property name="default_width">500</property>
  <property name="default_height">340</property>
  <property name="resizable">True</property>
  <property name="destroy_with_parent">False</property>
  <property name="decorated">True</property>
  <property name="skip_taskbar_hint">False</property>
  <property name="skip_pager_hint">False</property>
  <property name="type_hint">GDK_WINDOW_TYPE_HINT_NORMAL</property>
  <property name="gravity">GDK_GRAVITY_NORTH_WEST</property>
  <signal name="delete_event" handler="on_w_main_delete_event" last_modification_time="Sun, 19 Sep 2004 18:47:34 GMT"/>

  <child>
    <widget class="GtkVBox" id="vbox1">
      <property name="visible">True</property>
      <property name="homogeneous">False</property>
      <property name="spacing">0</property>

      <child>
	<widget class="GtkScrolledWindow" id="sw_aps">
	  <property name="visible">True</property>
	  <property name="can_focus">True</property>
	  <property name="hscrollbar_policy">GTK_POLICY_ALWAYS</property>
	  <property name="vscrollbar_policy">GTK_POLICY_ALWAYS</property>
	  <property name="shadow_type">GTK_SHADOW_NONE</property>
	  <property name="window_placement">GTK_CORNER_TOP_LEFT</property>

	  <child>
	    <widget class="GtkTreeView" id="tv_aps">
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="headers_visible">True</property>
	      <property name="rules_hint">False</property>
	      <property name="reorderable">False</property>
	      <property name="enable_search">True</property>
	    </widget>
	  </child>
	</widget>
	<packing>
	  <property name="padding">0</property>
	  <property name="expand">True</property>
	  <property name="fill">True</property>
	</packing>
      </child>

      <child>
	<widget class="GtkHBox" id="hbox1">
	  <property name="border_width">1</property>
	  <property name="visible">True</property>
	  <property name="homogeneous">False</property>
	  <property name="spacing">0</property>

	  <child>
	    <widget class="GtkButton" id="b_connect">
	      <property name="border_width">3</property>
	      <property name="width_request">100</property>
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <signal name="clicked" handler="on_b_connect_clicked" last_modification_time="Mon, 20 Sep 2004 02:06:30 GMT"/>

	      <child>
		<widget class="GtkAlignment" id="alignment1">
		  <property name="border_width">3</property>
		  <property name="visible">True</property>
		  <property name="xalign">0.5</property>
		  <property name="yalign">0.5</property>
		  <property name="xscale">0</property>
		  <property name="yscale">0</property>
		  <property name="top_padding">0</property>
		  <property name="bottom_padding">0</property>
		  <property name="left_padding">0</property>
		  <property name="right_padding">0</property>

		  <child>
		    <widget class="GtkHBox" id="hbox2">
		      <property name="visible">True</property>
		      <property name="homogeneous">False</property>
		      <property name="spacing">2</property>

		      <child>
			<widget class="GtkImage" id="image1">
			  <property name="visible">True</property>
			  <property name="stock">gtk-jump-to</property>
			  <property name="icon_size">4</property>
			  <property name="xalign">0.5</property>
			  <property name="yalign">0.5</property>
			  <property name="xpad">0</property>
			  <property name="ypad">0</property>
			</widget>
			<packing>
			  <property name="padding">0</property>
			  <property name="expand">False</property>
			  <property name="fill">False</property>
			</packing>
		      </child>

		      <child>
			<widget class="GtkLabel" id="label1">
			  <property name="visible">True</property>
			  <property name="label" translatable="yes">Connect</property>
			  <property name="use_underline">True</property>
			  <property name="use_markup">False</property>
			  <property name="justify">GTK_JUSTIFY_LEFT</property>
			  <property name="wrap">False</property>
			  <property name="selectable">False</property>
			  <property name="xalign">0.5</property>
			  <property name="yalign">0.5</property>
			  <property name="xpad">0</property>
			  <property name="ypad">0</property>
			</widget>
			<packing>
			  <property name="padding">0</property>
			  <property name="expand">False</property>
			  <property name="fill">False</property>
			</packing>
		      </child>
		    </widget>
		  </child>
		</widget>
	      </child>
	    </widget>
	    <packing>
	      <property name="padding">0</property>
	      <property name="expand">False</property>
	      <property name="fill">False</property>
	      <property name="pack_type">GTK_PACK_END</property>
	    </packing>
	  </child>

	  <child>
	    <widget class="GtkButton" id="b_rescan">
	      <property name="border_width">3</property>
	      <property name="width_request">100</property>
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <signal name="clicked" handler="on_b_rescan_clicked" last_modification_time="Mon, 20 Sep 2004 02:02:09 GMT"/>

	      <child>
		<widget class="GtkAlignment" id="alignment2">
		  <property name="visible">True</property>
		  <property name="xalign">0.5</property>
		  <property name="yalign">0.5</property>
		  <property name="xscale">0</property>
		  <property name="yscale">0</property>
		  <property name="top_padding">0</property>
		  <property name="bottom_padding">0</property>
		  <property name="left_padding">0</property>
		  <property name="right_padding">0</property>

		  <child>
		    <widget class="GtkHBox" id="hbox3">
		      <property name="visible">True</property>
		      <property name="homogeneous">False</property>
		      <property name="spacing">2</property>

		      <child>
			<widget class="GtkImage" id="image2">
			  <property name="visible">True</property>
			  <property name="stock">gtk-refresh</property>
			  <property name="icon_size">4</property>
			  <property name="xalign">0.5</property>
			  <property name="yalign">0.5</property>
			  <property name="xpad">0</property>
			  <property name="ypad">0</property>
			</widget>
			<packing>
			  <property name="padding">0</property>
			  <property name="expand">False</property>
			  <property name="fill">False</property>
			</packing>
		      </child>

		      <child>
			<widget class="GtkLabel" id="label2">
			  <property name="visible">True</property>
			  <property name="label" translatable="yes">Rescan</property>
			  <property name="use_underline">True</property>
			  <property name="use_markup">False</property>
			  <property name="justify">GTK_JUSTIFY_LEFT</property>
			  <property name="wrap">False</property>
			  <property name="selectable">False</property>
			  <property name="xalign">0.5</property>
			  <property name="yalign">0.5</property>
			  <property name="xpad">0</property>
			  <property name="ypad">0</property>
			</widget>
			<packing>
			  <property name="padding">0</property>
			  <property name="expand">False</property>
			  <property name="fill">False</property>
			</packing>
		      </child>
		    </widget>
		  </child>
		</widget>
	      </child>
	    </widget>
	    <packing>
	      <property name="padding">0</property>
	      <property name="expand">False</property>
	      <property name="fill">False</property>
	      <property name="pack_type">GTK_PACK_END</property>
	    </packing>
	  </child>

	  <child>
	    <widget class="GtkButton" id="b_quit">
	      <property name="border_width">3</property>
	      <property name="width_request">100</property>
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="label">gtk-quit</property>
	      <property name="use_stock">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <signal name="clicked" handler="gtk_main_quit" last_modification_time="Mon, 13 Sep 2004 06:21:54 GMT"/>
	    </widget>
	    <packing>
	      <property name="padding">0</property>
	      <property name="expand">False</property>
	      <property name="fill">False</property>
	      <property name="pack_type">GTK_PACK_END</property>
	    </packing>
	  </child>

	  <child>
	    <widget class="GtkButton" id="b_quit_unload">
	      <property name="border_width">3</property>
	      <property name="width_request">100</property>
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="label" translatable="yes">Quit+Unload</property>
	      <property name="use_underline">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <signal name="clicked" handler="on_b_quit_unload_clicked" last_modification_time="Mon, 20 Sep 2004 05:15:00 GMT"/>
	    </widget>
	    <packing>
	      <property name="padding">0</property>
	      <property name="expand">False</property>
	      <property name="fill">False</property>
	      <property name="pack_type">GTK_PACK_END</property>
	    </packing>
	  </child>
	</widget>
	<packing>
	  <property name="padding">0</property>
	  <property name="expand">False</property>
	  <property name="fill">True</property>
	</packing>
      </child>
    </widget>
  </child>
</widget>

<widget class="GtkDialog" id="dlg_passwd">
  <property name="title" translatable="yes">Password</property>
  <property name="type">GTK_WINDOW_TOPLEVEL</property>
  <property name="window_position">GTK_WIN_POS_NONE</property>
  <property name="modal">False</property>
  <property name="resizable">True</property>
  <property name="destroy_with_parent">False</property>
  <property name="decorated">True</property>
  <property name="skip_taskbar_hint">False</property>
  <property name="skip_pager_hint">False</property>
  <property name="type_hint">GDK_WINDOW_TYPE_HINT_DIALOG</property>
  <property name="gravity">GDK_GRAVITY_NORTH_WEST</property>
  <property name="has_separator">True</property>

  <child internal-child="vbox">
    <widget class="GtkVBox" id="dialog-vbox1">
      <property name="visible">True</property>
      <property name="homogeneous">False</property>
      <property name="spacing">0</property>

      <child internal-child="action_area">
	<widget class="GtkHButtonBox" id="dialog-action_area1">
	  <property name="visible">True</property>
	  <property name="layout_style">GTK_BUTTONBOX_END</property>

	  <child>
	    <widget class="GtkButton" id="b_pass_cancel">
	      <property name="visible">True</property>
	      <property name="can_default">True</property>
	      <property name="can_focus">True</property>
	      <property name="label">gtk-cancel</property>
	      <property name="use_stock">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <property name="response_id">-6</property>
	    </widget>
	  </child>

	  <child>
	    <widget class="GtkButton" id="b_pass_ok">
	      <property name="visible">True</property>
	      <property name="can_default">True</property>
	      <property name="can_focus">True</property>
	      <property name="label">gtk-ok</property>
	      <property name="use_stock">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <property name="response_id">-5</property>
	    </widget>
	  </child>
	</widget>
	<packing>
	  <property name="padding">0</property>
	  <property name="expand">False</property>
	  <property name="fill">True</property>
	  <property name="pack_type">GTK_PACK_END</property>
	</packing>
      </child>

      <child>
	<widget class="GtkVBox" id="vbox2">
	  <property name="visible">True</property>
	  <property name="homogeneous">False</property>
	  <property name="spacing">0</property>

	  <child>
	    <widget class="GtkLabel" id="lb_passwd">
	      <property name="visible">True</property>
	      <property name="label" translatable="yes">Enter WEP key / password</property>
	      <property name="use_underline">False</property>
	      <property name="use_markup">False</property>
	      <property name="justify">GTK_JUSTIFY_LEFT</property>
	      <property name="wrap">False</property>
	      <property name="selectable">False</property>
	      <property name="xalign">0.5</property>
	      <property name="yalign">0.5</property>
	      <property name="xpad">0</property>
	      <property name="ypad">0</property>
	    </widget>
	    <packing>
	      <property name="padding">0</property>
	      <property name="expand">False</property>
	      <property name="fill">False</property>
	    </packing>
	  </child>

	  <child>
	    <widget class="GtkEntry" id="te_passwd">
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="editable">True</property>
	      <property name="visibility">True</property>
	      <property name="max_length">0</property>
	      <property name="text" translatable="yes"></property>
	      <property name="has_frame">True</property>
	      <property name="invisible_char" translatable="yes">*</property>
	      <property name="activates_default">False</property>
	    </widget>
	    <packing>
	      <property name="padding">0</property>
	      <property name="expand">False</property>
	      <property name="fill">False</property>
	    </packing>
	  </child>

	  <child>
	    <widget class="GtkRadioButton" id="rb_wep">
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="label" translatable="yes">This is a WEP key</property>
	      <property name="use_underline">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <property name="active">False</property>
	      <property name="inconsistent">False</property>
	      <property name="draw_indicator">True</property>
	    </widget>
	    <packing>
	      <property name="padding">0</property>
	      <property name="expand">False</property>
	      <property name="fill">False</property>
	    </packing>
	  </child>

	  <child>
	    <widget class="GtkRadioButton" id="rb_password">
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="label" translatable="yes">This is a password</property>
	      <property name="use_underline">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <property name="active">False</property>
	      <property name="inconsistent">False</property>
	      <property name="draw_indicator">True</property>
	      <property name="group">rb_wep</property>
	    </widget>
	    <packing>
	      <property name="padding">0</property>
	      <property name="expand">False</property>
	      <property name="fill">False</property>
	    </packing>
	  </child>
	</widget>
	<packing>
	  <property name="padding">0</property>
	  <property name="expand">True</property>
	  <property name="fill">True</property>
	</packing>
      </child>
    </widget>
  </child>
</widget>

<widget class="GtkDialog" id="dlg_connected">
  <property name="can_default">True</property>
  <property name="title" translatable="yes">Connected</property>
  <property name="type">GTK_WINDOW_TOPLEVEL</property>
  <property name="window_position">GTK_WIN_POS_NONE</property>
  <property name="modal">True</property>
  <property name="default_width">400</property>
  <property name="default_height">300</property>
  <property name="resizable">True</property>
  <property name="destroy_with_parent">False</property>
  <property name="decorated">True</property>
  <property name="skip_taskbar_hint">False</property>
  <property name="skip_pager_hint">False</property>
  <property name="type_hint">GDK_WINDOW_TYPE_HINT_DIALOG</property>
  <property name="gravity">GDK_GRAVITY_NORTH_WEST</property>
  <property name="has_separator">True</property>

  <child internal-child="vbox">
    <widget class="GtkVBox" id="dialog-vbox2">
      <property name="visible">True</property>
      <property name="homogeneous">False</property>
      <property name="spacing">0</property>

      <child internal-child="action_area">
	<widget class="GtkHButtonBox" id="dialog-action_area2">
	  <property name="visible">True</property>
	  <property name="layout_style">GTK_BUTTONBOX_END</property>

	  <child>
	    <widget class="GtkButton" id="b_con_cancel">
	      <property name="visible">True</property>
	      <property name="can_default">True</property>
	      <property name="can_focus">True</property>
	      <property name="label">gtk-cancel</property>
	      <property name="use_stock">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <property name="response_id">-6</property>
	    </widget>
	  </child>

	  <child>
	    <widget class="GtkButton" id="b_con_ok">
	      <property name="visible">True</property>
	      <property name="can_default">True</property>
	      <property name="has_default">True</property>
	      <property name="can_focus">True</property>
	      <property name="has_focus">True</property>
	      <property name="label">gtk-ok</property>
	      <property name="use_stock">True</property>
	      <property name="relief">GTK_RELIEF_NORMAL</property>
	      <property name="focus_on_click">True</property>
	      <property name="response_id">-5</property>
	    </widget>
	  </child>
	</widget>
	<packing>
	  <property name="padding">0</property>
	  <property name="expand">False</property>
	  <property name="fill">True</property>
	  <property name="pack_type">GTK_PACK_END</property>
	</packing>
      </child>

      <child>
	<widget class="GtkScrolledWindow" id="sw_connected">
	  <property name="visible">True</property>
	  <property name="can_focus">True</property>
	  <property name="hscrollbar_policy">GTK_POLICY_ALWAYS</property>
	  <property name="vscrollbar_policy">GTK_POLICY_ALWAYS</property>
	  <property name="shadow_type">GTK_SHADOW_NONE</property>
	  <property name="window_placement">GTK_CORNER_TOP_LEFT</property>

	  <child>
	    <widget class="GtkTreeView" id="tv_connected">
	      <property name="visible">True</property>
	      <property name="can_focus">True</property>
	      <property name="headers_visible">True</property>
	      <property name="rules_hint">False</property>
	      <property name="reorderable">False</property>
	      <property name="enable_search">True</property>
	    </widget>
	  </child>
	</widget>
	<packing>
	  <property name="padding">0</property>
	  <property name="expand">True</property>
	  <property name="fill">True</property>
	</packing>
      </child>
    </widget>
  </child>
</widget>

</glade-interface>

