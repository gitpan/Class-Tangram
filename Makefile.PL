use ExtUtils::MakeMaker;

my ($VERSION, $NAME, $PREREQ_PM, $ABSTRACT, $AUTHOR,
    @WriteMakeFileOptions);

require 'Makefile.cfg';

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
unless ( $NO_AUTO ) {

    eval "use Pod::Constants";

    if ( $@  or $Pod::Constants::VERSION < 0.11 ) {

	warn "Warning: prerequisite Pod::Constants failed to load: $@"
	    if $@;
	$PREREQ_PM = { 'Pod::Constants' => 0.11 };

	open MODULE, "<$ALL_FROM"
	    or die "Couldn't open $ALL_FROM for reading; $!";
	while (<MODULE>) {
	    m/^package ([^;]*);/ && do { $NAME = $1; last };
	}
	close MODULE;

    } else {

	Pod::Constants::import_from_file 
	    (
	     $ALL_FROM,
	     'MODULE RELEASE' => sub { ($VERSION) = m/(\d+\.\d+)/ },
	     'DEPENDENCIES' => ($PREREQ_PM = { }),
	     -trim => 1,
	     'NAME' => sub { $ABSTRACT=$_; ($NAME) = m/(\S+)/ },
	     'AUTHOR' => \$AUTHOR,
	    );

    }
}

WriteMakefile
    (
     'PREREQ_PM'		=> $PREREQ_PM,
     ($NAME    ? ('NAME'	=> $NAME) : ()),
     ($VERSION ? ('VERSION'	=> $VERSION) : ()),
     ( ($] >= 5.005) && $ABSTRACT ?
       ## Add these new keywords supported since 5.005
      (ABSTRACT                 => $ABSTRACT,
       AUTHOR                   => $AUTHOR) : ()),
     @WriteMakeFileOptions,
    );

