use ExtUtils::MakeMaker;

my ($VERSION, $NAME, $PREREQ_PM, $ABSTRACT, $AUTHOR,
    @WriteMakeFileOptions);

WriteMakefile
    (
     'PREREQ_PM'		=> {
		   Set::Object => 1.02,
   Test::Simple => 0.18,
   Date::Manip => 5.21,
	},
     'NAME'	=> "Class::Tangram",
     'VERSION'	=> "1.13",
     ( ($] >= 5.005) && $ABSTRACT ?
       ## Add these new keywords supported since 5.005
      (ABSTRACT                 => "Automatic constructor, accessors & SQL DB mapping",
       AUTHOR                   => "Sam Vilain <sam@vilain.net>") : ()),
     @WriteMakeFileOptions,
    );

