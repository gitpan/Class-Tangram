use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
eval "use Pod::Constants";
warn "Warning: prerequisite Pod::Constants failed to load: $@" if $@;

($Pod::Constants::VERSION >= 0.11)
    or die <<EOF;
####
####  ERROR: This module requires Pod::Constants 0.11 or higher to be
####  installed.
####
EOF

my ($VERSION, $NAME, $PREREQ_PM, $ABSTRACT, $AUTHOR);
Pod::Constants::import_from_file 
    (
     'Tangram.pm',
     'MODULE RELEASE' => sub { ($VERSION) = m/(\d+\.\d+)/ },
     'DEPENDENCIES' => ($PREREQ_PM = { }),
     -trim => 1,
     'NAME' => sub { $ABSTRACT=$_; ($NAME) = m/(\S+)/ },
     'AUTHOR' => \$AUTHOR,
    );

WriteMakefile
    (
     'NAME'		=> $NAME,
     'PREREQ_PM'        => $PREREQ_PM,
     'VERSION'          => $VERSION,
     ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (ABSTRACT         => $ABSTRACT,
       AUTHOR           => $AUTHOR) : ()),
    );

