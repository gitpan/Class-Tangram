#   Hey, emacs - this is -*- perl -*- !!
#
#  test script for Tangram::Object
#

use strict;
use Test::More tests => 34;

#---------------------------------------------------------------------
# Test 1:   Check Class::Tangram loads

use_ok("Class::Tangram");
use_ok("Set::Object");

#---------------------------------------------------------------------
# define our movie database
package Movie;
use vars qw(@ISA $schema);
@ISA = qw(Class::Tangram);

$schema = {
	   fields => {
		      string => [ qw(title) ],
		      int => [ qw(release_year) ],
		      # this means there is a set of 'Credit' objects
		      # related to this 'Movie' object.
		      iset =>
		      {
		       credits => 'Credit',
		      },
		     },
	  };

package Person;
use vars qw(@ISA $schema);
@ISA = qw(Class::Tangram);
$schema = {
	   fields => {
		      string => [ qw(name) ],
		      rawdatetime => [ qw(birthdate) ],
		      ref => [ qw(birth_location) ],
		      real => { height => undef, },
		      # This person also has a set of credits
		      iset =>
		      {
		       credits => 'Credit',
		      },
		     },
	  };

package Job;
use vars qw(@ISA $schema);
@ISA = qw(Class::Tangram);
$schema = {
	   fields => {
		      string => [ qw(job_title) ],
		      # As does this job
		      iset =>
		      {
		       credits => 'Credit',
		      },
		     }
	  };

package Credit;
use vars qw($schema);
use base qw(Class::Tangram);

my $counter;

$schema = {
	   fields => {
		      # nothing, this is an association class with no data.
		      # these fields are used for later tests
		      string =>
		      {
		       foo => {
			       check_func => sub {
				   die if (${$_[0]} ne "bar");
				   },
			       init_default => "baz",
			       },
		       bar => {
			       init_default => sub {
				   ++$counter;
				   }
			       }
		      },
		      int =>
		      {
		       cheese => {
				  check_func => sub {
				      die "too big" if (${$_[0]} > 15);
				  },
				  init_default => 15,
				 },
		      },
		     },
	  };

package Location;
use vars qw(@ISA $schema);
@ISA = qw(Class::Tangram);
$schema = {
	   fields => {
		      string => [ qw(location) ],
		      ref => [ qw(parent_location) ],
		     }
	  };

#---------------------------------------------------------------------
package main;
use strict;

for my $pkg (qw(Movie Person Job Credit Location)) {
    eval { Class::Tangram::import_schema($pkg) };
    is($@, "", "import_schema('$pkg')");
}

my (@locations, @credits, @jobs, @movies, @actors);

eval {
    @locations =
        (
         new Location( location => "Grappenhall",
                       parent_location => new Location
                       ( location => "Warrington",
                         parent_location => new Location
                         ( location => "Cheshire",
                           parent_location => new Location
                           ( location => "England",
                             parent_location => new Location
                             ( location => "United Kingdom" ) ) ) ) ),
         new Location( location => "Dallas",
		       parent_location => new Location
                       ( location => "Texas",
                         parent_location => new Location
                         ( location => "United States" ) ) ),
	);

    @credits = ( map { new Credit } (1..5) );

    @jobs =
	(
	 new Job( job_title => "Dr. Frank-N-Furter",
		  credits => Set::Object->new( $credits[0] ) ),
	 new Job( job_title => "Wadsworth",
		  credits => Set::Object->new( $credits[1] ) ),
	 new Job( job_title => "Prosecutor",
		  credits => Set::Object->new( $credits[2] ) ),
	 new Job( job_title => "Long John Silver",
		  credits => Set::Object->new( $credits[3] ) ),
	 new Job( job_title => "Dr. Scott",
		  credits => Set::Object->new( $credits[4] ) ),
	);

    @movies =
	(
	 new Movie( title => "Rocky Horror Picture Show",
		    release_year => 1975,
		    credits => Set::Object->new( @credits[0, 4] ) ),
	 new Movie( title => "Clue",
		    release_year => 1985,
		    credits => Set::Object->new( $credits[1] ) ),
	 new Movie( title => "The Wall: Live in Berlin",
		    release_year => 1990,
		    credits => Set::Object->new( $credits[2] ) ),
	 new Movie( title => "Muppet Treasure Island",   
		    release_year => 1996,
		    credits => Set::Object->new( $credits[3] ) ),
	);

    @actors =
	(
	 new Person( name => "Tim Curry",
		     birthdate => "1946-04-19 12:00:00",
		     birth_location => $locations[0],
		     credits =>
		     Set::Object->new( @credits[0..3] ) ),
	 new Person( name => "Marvin Lee Aday",
		     birthdate => "1947-09-27 12:00:00",
		     birth_location => $locations[1],
		     credits =>
		     Set::Object->new( $credits[4] ) ),
	);

};

is($@, "", "new of various objects");

is($locations[0]->location, "Grappenhall", "new Location");

#---------------------------------------------------------------------
#  test set

# string
eval { $actors[0]->set_name("Timothy Curry"); };
is ($@, "", "Set string to legal value");

eval { $actors[0]->set_name("Tim Curry" x 100); };
isnt ($@, "", "Set string to illegal value");

# int
eval { $movies[0]->set_release_year("-2000"); };
is ($@, "", "Set int to legal value");

eval { $movies[0]->set_release_year("2000BC"); };
isnt ($@, "", "Set int to illegal value");

# real
eval {
    $actors[0]->set_height("1.3e7");
    $actors[0]->set_height("1.3");
    $actors[0]->set_height("-12345678735");
};
is ($@, "", "Set real to legal value");

eval { $actors[0]->set_height("12345i"); };
isnt ($@, "", "Set real to illegal value");

# obj
eval {
    $actors[1]->set_birth_location($locations[int rand scalar @locations]);
    $actors[1]->set_birth_location($locations[int rand scalar @locations]);
    $actors[1]->set_birth_location($locations[int rand scalar @locations]);
};
is ($@, "", "Set ref to legal value");

eval { $actors[0]->set_birth_location("Somewhere, over the rainbow"); };
isnt ($@, "", "Set ref to illegal value");

# flat array

# array

# set

# rawdatetime

# time

# timestamp

# flat_hash

# hash

#---------------------------------------------------------------------
# check init_default
is($credits[0]->foo, "baz", "init_default scalar");
is($credits[0]->bar, 1, "init_default sub");
is($credits[3]->bar, 4, "init_default sub");

# need to check hash, array versions... later

#---------------------------------------------------------------------
# check check_func
eval {
    $credits[0]->set_foo("Anything");
};
isnt($@, "", "check_func string illegal");
eval {
    $credits[0]->set_foo("bar");
};
is($@, "", "check_func string legal");

eval { $credits[0]->set_cheese(16); };
isnt($@, "", "check_func int illegal");
eval { $credits[0]->set_cheese(-1); };
is($@, "", "check_func int legal");

#---------------------------------------------------------------------
#  check clear_refs
my $movie = new Movie;
$movie->{credits} = "something illegal";
eval { $movie->clear_refs(); };
is($@, "", "clear_refs on bogus set OK");

#---------------------------------------------------------------------
# check get on invalid fields
eval { $actors[0]->set_karma("high"); };
isnt($@, "", "Set invalid field");
eval { $locations[0]->set("cheese", "high"); };
isnt($@, "", "Set invalid field");

#---------------------------------------------------------------------
# Set::Object functions
my @foo = $actors[0]->credits;
is ($#foo, 3, "list context get of set");
my $foo = $actors[0]->credits;
ok($foo->isa("Set::Object"), "scalar context get of set");
ok($actors[0]->credits_includes($foo[2]), "AUTOLOAD _includes");
$actors[0]->credits_remove($foo[2]);
ok(!$actors[0]->credits_includes($foo[2]), "AUTOLOAD _remove");
$actors[0]->credits_clear;
ok(!$actors[0]->credits_includes($foo[1]), "AUTOLOAD _clear");
$actors[0]->credits_insert($foo[1]);
ok($actors[0]->credits_includes($foo[1]), "AUTOLOAD _insert");
is($actors[0]->credits_size, 1, "AUTOLOAD _size");
