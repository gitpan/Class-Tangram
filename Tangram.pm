package Class::Tangram;

# Copyright (c) 2001, 2002 Sam Vilain.  All right reserved.  This file
# is licensed under the terms of the Perl Artistic license.

# Some modifications
# Copyright �� 2001 Micro Sharp Technologies, Inc., Vancouver, WA, USA
# Author: Karl M. Hegbloom <karlheg@microsharp.com>
# Perl Artistic Licence.

=head1 NAME

Class::Tangram - create constructors, accessor and update methods for
objects from a Tangram-compatible object specification.

=head1 SYNOPSIS

 package Orange;

 use base qw(Class::Tangram);
 use vars qw($schema);
 use Tangram::Ref;

 # define the schema (ie, allowed attributes) of this
 # object.  See the Tangram::Schema man page for an
 # introduction to the Tangram schema syntax.

 $schema = {
     table => "oranges",

     fields => {
         int => {
             juiciness => undef,
             segments => {
                 # this code reference is called when this
                 # attribute is set, to check the value is
                 # OK
                 check_func => sub {
                     die "too many segments"
                         if (${ shift } > 30);
                 },
                 # the default for this attribute.
                 init_default => 7,
             },
         },
         ref => {
             grower => undef,
         },
         # fields allowed by Class::Tangram but not ever
         # stored - no type checking
         transient => [ qw(_tangible) ],
     },
 };
 Class::Tangram::import_schema("Orange");

 package Project;
 # here's where we build the individual object schemas into a
 # Tangram::Schema object, which the Tangram::Storage class uses to
 # know which tables and columns to find objects.
 use Tangram::Schema;

 my $dbschema = Tangram::Schema->new
     ({ classes => [ 'Orange' => $Orange::schema ]});

 sub schema { $dbschema };

 package main;

 # See Tangram::Relational for instructions on using "deploy" to
 # create the database this connects to.  You only have to do this if
 # you want to write the objects to a database.
 use Tangram::Relational;
 my ($dsn, $u, $p);
 my $storage = Tangram::Relational->connect(Project->schema,
                                            $dsn, $u, $p);

 # This is how you create instances
 my $orange = Orange->new(juiciness => 8);

 # Store them
 $storage->insert($orange);

 # This is how you get values out of the objects
 my $juiciness = $orange->juiciness;

 # a "ref" must be set to a blessed object, any object
 my $grower = bless { name => "Joe" }, "Farmer";
 $orange->set_grower ($grower);

 # these are all illegal - type checking is fairly strict
 eval { $orange->set_juiciness ("Yum"); };   print $@;
 eval { $orange->set_segments (31); };       print $@;
 eval { $orange->set_grower ("Mr. Nice"); }; print $@;

 # if you prefer
 $orange->get( "juiciness" );
 $orange->set( juiciness => 123 );

 # Re-configure init_default
 $orange->set_init_default( juiciness => sub { int(rand(45)) } );

=head1 DESCRIPTION

Class::Tangram is a common base class originally intended for use with
Tangram objects, that gives you free constructors, access methods,
update methods, and a destructor that should help in breaking circular
references for you. Type checking is achieved by parsing the Tangram
schema for the object, which is contained within the object class in
an exported variable C<$schema>.

After writing this I found that it was useful for merely adding type
checking and validation to arbitrary objects.  There are several
modules on CPAN to do that already, but many don't have finely grained
type checking, and none of them integrated with Tangram.

=head1 DEPENDENCIES

The following modules are required to be installed to use
Class::Tangram:

   Set::Object => 1.02
   Pod::Constants => 0.11
   Test::Simple => 0.18
   Date::Manip => 5.21

Test::Simple and Date::Manip are only required to run the test suite.

If you find Class::Tangram passes the test suite with earlier versions
of the above modules, please send me an e-mail.

=head2 MODULE RELEASE

This is Class::Tangram version 1.08.

=cut

use strict;
use Carp qw(croak cluck);

use vars qw($AUTOLOAD $VERSION %defaults);

use Set::Object;
use Pod::Constants -trim => 1,
    'MODULE RELEASE' => sub { ($VERSION) = /(\d+\.\d+)/ },
    'TANGRAM TYPE TO FUNCTION MAPPING' => sub { %defaults = eval };

local $AUTOLOAD;

# $types{$class}->{$attribute} is the tangram type of each attribute
my (%types);

# $attribute_options{$class}->{$attribute} is the hash passed to tangram
# for the given attribute
my (%attribute_options);

# $check{$class}->{$attribute}->($value) is a function that will die
# if $value is not alright, see check_X functions
my (%check);

# Destructors for each attribute.  They are called as
# $cleaners{$class}->{$attribute}->($self, $attribute);
my (%cleaners);

# init_default values for each attribute.  These could be hash refs,
# array refs, code refs, or simple scalars.  They will be stored as
# $init_defaults{$class}->{$attribute}
my (%init_defaults);

# if a class is abstract, complain if one is constructed.
my (%abstract);

=head1 METHODS

=over 4

=item Class-E<gt>new (attribute1 =E<gt> value, attribute2 =E<gt> value)

sets up a new object of type Class, with attributes set to the values
supplied.

Can also be used as an object method, in which case it returns a
B<copy> of the object, without any deep copying.

=cut

sub new ($@)
{
    my $invocant = shift;
    my $class = ref $invocant || $invocant;

    (my @values, @_) = @_;

    # Setup the object
    my $self = { };
    bless $self, $class;

    exists $check{$class} or import_schema($class);

    croak "Attempt to instantiate an abstract type"
	if ($abstract{$class});

    if ($invocant ne $class)
    {
	# The copy constructor; this could be better :)
	# this has the side effect of much auto-vivification.
	%$self = %$invocant;
	$self->set (@values); # override with @values
    }
    else
    {
	$self->set (@values); # start with @values

	# now fill in fields that have defaults
	for my $attribute (keys %{$init_defaults{$class}}) {

	    next if (exists $self->{$attribute});

	    my $default = $init_defaults{$class}->{$attribute}
		unless tied $init_defaults{$class}->{$attribute};

	    if (ref $default eq "CODE") {
		# sub { }, attribute gets return value
		$self->{$attribute}
		    = $init_defaults{$class}->{$attribute}->($self);

	    } elsif (ref $default eq "HASH") {
		# hash ref, copy hash
		$self->{$attribute}
		    = { %{ $init_defaults{$class}->{$attribute} } };

	    } elsif (ref $default eq "ARRAY") {
		# array ref, copy array
		$self->{$attribute}
		    = [ @{ $init_defaults{$class}->{$attribute} } ];

	    } else {
		# something else, an object or a scalar
		$self->{$attribute}
		    = $init_defaults{$class}->{$attribute};
	    }
	}
    }
    return $self;
}

=item $instance->set(attribute => $value, ...)

Sets the attributes of the given instance to the given values.  croaks
if there is a problem with the values.

=cut

sub set($@) {
    my $self = shift;

    # yes, this is a lot to do.  yes, it's slow.  But I'm fairly
    # certain that this could be handled efficiently if it were to be
    # moved inside the Perl interpreter or an XS module
    UNIVERSAL::isa($self, "Class::Tangram") or croak "type mismatch";
    my $class = ref $self;
    exists $check{$class} or import_schema($class);

    while (my ($name, $value) = splice @_, 0, 2) {
	croak "attempt to set an illegal field $name in a $class"
	    if (!defined $check{$class}->{$name});

	#local $@;

	# these handlers die on failure
	eval { $check{$class}->{$name}->(\$value) };
	$@ && croak ("value failed type check - ${class}->{$name}, "
		     ."\"$value\" ($@)");

	#should be ok now
	$self->{$name} = $value;
    }
}

=item $instance->get($attribute)

Gets the value of $attribute.  If the attribute in question is a set,
and this method is called in list context, then it returns the MEMBERS
of the set (if called in scalar context, it returns the Set::Object
container).

=cut

sub get($$) {
    my $self = shift;
    my $field = shift;
    UNIVERSAL::isa($self, "Class::Tangram") or croak "type mismatch";

    my $class = ref $self;
    exists $check{$class} or import_schema($class);
    croak "attempt to read an illegal field $field in a $class"
	if (!defined $check{$class}->{$field});

    # make sure we don't ever return "undef" for fields that might be
    # used as a reference, and return contents of containers when in
    # list context
    if ((local $_ = $types{$class}->{$field}) =~ m/^i?set$/o) {

	$self->{$field} ||= Set::Object->new();
	return $self->{$field}->members if wantarray;

    } elsif (m/^i?array$/o) {

	$self->{$field} ||= [];
	return @{ $self->{$field} } if wantarray;

    } elsif ( m/^i?hash$/o)  {

	$self->{$field} ||= {};
	return %{ $self->{$field} } if wantarray;
    }

    return $self->{$field};
}

=item $instance->attribute($value)

If $value is not given, then
this is equivalent to $instance->get("attribute")

If $value is given, then this is equivalent to
$instance->set("attribute", $value).  This usage issues a warning; you
should change your code to use the set_attribute syntax for better
readability.

=item $instance->get_attribute

=item $instance->set_attribute($value)

Equivalent to $instance->get("attribute") and $instance->set(attribute
=> $value), respectively.

=item $instance->attribute_includes(@objects)

=item $instance->attribute_insert(@objects)

=item $instance->attribute_size

=item $instance->attribute_clear

=item $instance->attribute_remove(@objects)

Equivalent to calling $instance->attribute->includes(@objects), etc.
This only works if the attribute in question is a Set::Object.

=cut

sub AUTOLOAD {
    my $self = shift;
    UNIVERSAL::isa($self, "Class::Tangram")
	    or croak "type mismatch/$self->$AUTOLOAD undefined";

    my $class = ref $self;
    exists $check{$class} or import_schema($class);
    $AUTOLOAD =~ s/.*://;
    if ($AUTOLOAD =~ m/^(set_|get_)?([^:]+)$/
	and defined $types{$class}->{$2}) {

	# perl sucks at this type of test
	my $value = shift;
	if ((defined $1 and $1 eq "set_")
	    or (!defined $1 and defined $value)) {

	    if ($^W && !defined $1) {
		cluck("The OO police say change your call to "
		      ."\$obj->set_$2");
	    }
	    return set($self, $2, $value);
	} else {
	    return get($self, $2);
	}
    } elsif (my ($attr, $method) =
	     ($AUTOLOAD =~ m/^(.*)_(includes|insert|
			     size|clear|remove)$/x)
	     and $types{$class}->{$1} =~ m/^i?set$/) {
	# would like to use GOTO here, but it segfaults
	return $self->get($attr)->$method(@_);
	#unshift @_, $self;
	#my $x;
	#if ( $x = (get($self, $attr))->can($method) ) {
	    #print ref($x), "\n";
	    #goto $x;
	#} else {
	    #return undef;
	#}
    } else {
	croak("unknown method/attribute ${class}->$AUTOLOAD called");
    }
}

=item $instance->getset($attribute, $value)

If you're replacing the AUTOLOAD function in your Class::Tangram
derived class, but would still like to use the behaviour for one or
two fields, then you can define functions for them to fall through to
the Class::Tangram method, like so:

 sub attribute { $_[0]->SUPER::getset("attribute", $_[1]) }

=cut

sub getset($$;$) {
    UNIVERSAL::isa($_[0], "Class::Tangram") or croak "type mismatch";

    if ($#_ > 1) {
	goto &set;
    } else {
	goto &get;
    }

}

=item check_X (\$value)

This series of internal functions checks that $value is of the type X,
and within applicable bounds.  If there is a problem, then it will
croak() the error.  These functions are not called from the code, but
by the set() method on a particular attribute.

When Class::Tangram comes across a type in the schema, it first sets
up the per-attribute code references to the "check" and "destroy"
functions.  If there is no check/destroy in the below table, there
must be a "parse", which is a parse_X function.  See parse_X, below

=head2 TANGRAM TYPE TO FUNCTION MAPPING

 int         => { check => \&check_int },
 real        => { check => \&check_real },
 string      => { parse => \&parse_string },
 ref         => { check => \&check_obj,
		  destroy => \&destroy_ref },
 array       => { check => \&check_array,
		  destroy => \&destroy_array },
 iarray      => { check => \&check_array,
		  destroy => \&destroy_array },
 flat_array  => { check => \&check_flat_array },
 set         => { check => \&check_set,
		  destroy => \&destroy_set },
 iset        => { check => \&check_set,
		  destroy => \&destroy_set },
 dmdatetime  => { check => \&check_dmdatetime },
 rawdatetime => { check => \&check_rawdatetime },
 rawdate     => { check => \&check_rawdate },
 rawtime     => { check => \&check_rawtime },
 flat_hash   => { check => \&check_flat_hash },
 transient   => { check => \&check_nothing },
 hash        => { check => \&check_hash,
		  destroy => \&destroy_hash },
 perl_dump   => { check => \&check_nothing }

=over

=item check_string

checks that the supplied value is less than 255 characters long.

=cut

sub check_string {
    croak "string ${$_[0]} too long"
	if (length ${$_[0]} > 255);
}

=item check_int

checks that the value is a (possibly signed) integer

=cut

my $int_re = qr/^-?\d+$/;
sub check_int {
    croak "${$_[0]} is not an int"
	if (${$_[0]} !~ m/$int_re/o);
}

=item check_real 

checks that the value is a real number, by stringifying it and
matching it against (m/^-?\d*(\.\d*)?(e-?\d*)?$/).  In the future, a
test that isn't a bunch of arse will be used.

=cut

my $real_re = qr/^-?\d*(\.\d*)?(e-?\d*)?$/;
sub check_real {
    croak "${$_[0]} is not a real"
	if (${$_[0]} !~ m/$real_re/o);
}

=item check_obj

checks that the supplied variable is a reference to a blessed object

=cut

# this pattern matches a regular reference
my $obj_re = qr/^(?:HASH|ARRAY|SCALAR)?$/;
sub check_obj {
    croak "not an object reference"
	if ((ref ${$_[0]}) =~ m/$obj_re/o);
}

=item check_flat_array

checks that $value is a ref ARRAY and that all elements are unblessed
scalars.  Does NOT currently check that all values are of the correct
type (int vs real vs string, etc)

=cut

sub check_flat_array {
    croak "not a flat array"
	if (ref ${$_[0]} ne "ARRAY");
    croak "flat array may not contain references"
	if (map { (ref $_ ? "1" : ()) } @{$_[0]});
}

=item check_array

checks that $value is a ref ARRAY, and that each element in the array
is a reference to a blessed object.

=cut

sub check_array {
    croak "array attribute not passed an array ref"
	if (ref ${$_[0]} ne "ARRAY");
    for my $a (@{${$_[0]}}) {
	croak "member in array not an object reference"
	    if ((ref $a) =~ m/$obj_re/o);
    }
}

=item check_set

checks that $value->isa("Set::Object")

=cut

sub check_set {
    croak "set type not passed a Set::Object"
	unless (UNIVERSAL::isa(${$_[0]}, "Set::Object"));
}

=item check_rawdate

checks that $value is of the form YYYY-MM-DD, or YYYYMMDD, or YYMMDD.

=cut

# YYYY-MM-DD HH:MM:SS
my $rawdate_re = qr/^(?:  \d{4}-\d{2}-\d{2}
                     |    (?:\d\d){3,4}
                     )$/x;
sub check_rawdate {
    croak "invalid SQL rawdate"
	unless (${$_[0]} =~ m/$rawdate_re/o);
}

=item check_rawtime

checks that $value is of the form HH:MM(:SS)?

=cut

# YYYY-MM-DD HH:MM:SS
my $rawtime_re = qr/^\d{1,2}:\d{2}(?::\d{2})?$/;
sub check_rawtime {
    croak "invalid SQL rawtime"
	unless (${$_[0]} =~ m/$rawtime_re/o);
}

=item check_rawdatetime

checks that $value is of the form YYYY-MM-DD HH:MM(:SS)? (the time
and/or the date can be missing), or a string of numbers between 6 and
14 numbers long.

=cut

my $rawdatetime_re = qr/^(?:
			    # YYYY-MM-DD HH:MM:SS
		            (?: (?:\d{4}-\d{2}-\d{2}\s+)?
		                \d{1,2}:\d{2}(?::\d{2})?
			    |   \d{4}-\d{2}-\d{2}
			    )
		         |  # YYMMDD, etc
		            (?:\d\d){3,7}
		         )$/x;
sub check_rawdatetime {
    croak "invalid SQL rawdatetime dude"
	unless (${$_[0]} =~ m/$rawdatetime_re/o);
}

=item check_dmdatetime

checks that $value is of the form YYYYMMDDHH:MM:SS, or those allowed
for rawdatetime.

=cut

sub check_dmdatetime {
    croak "invalid SQL rawdatetime dude"
	unless (${$_[0]} =~ m/^\d{10}:\d\d:\d\d$|$rawdatetime_re/o);
}

=item check_flat_hash

checks that $value is a ref HASH and all values are scalars.  Does NOT
currently check that all values are of the correct type (int vs real
vs string, etc)

=cut

sub check_flat_hash {
    croak "not a hash"
	unless (ref ${$_[0]} eq "HASH");
    while (my ($k, $v) = each %${$_[0]}) {
	croak "hash not flat"
	    if (ref $k or ref $v);
    }
}

=item check_hash

checks that $value is a ref HASH, that every key in the hash is a
scalar, and that every value is a blessed object.

=cut

sub check_hash {
    croak "not a hash"
	unless (ref ${$_[0]} eq "HASH");
    while (my ($k, $v) = each %${$_[0]}) {
	croak "hash key not flat"
	    if (ref $k);
	croak "hash value not an object"
	    if (ref $v !~ m/$obj_re/);
    }
}

=item check_nothing

checks whether Australians like sport

=back

=cut

sub check_nothing { }

=item destroy_X ($instance, $attr)

Similar story with the check_X series of functions, these are called
during object destruction on every attribute that has a reference that
might need breaking.  Note: B<these functions all assume that
attributes belonging to an object that is being destroyed may be
destroyed also>.  In other words, do not allow distinct objects to
share Set::Object containers or hash references in their attributes,
otherwise when one gets destroyed the others will lose their data.

Available functions:

=over

=item destroy_array

empties an array

=cut

sub destroy_array {
    my $self = shift;
    my $attr = shift;
    my $t = tied $self->{$attr};
    @{$self->{$attr}} = ()
	unless (defined $t and $t =~ m,Tangram::CollOnDemand,);
    delete $self->{$attr};
}

=item destroy_set

Calls Set::Object::clear to clear the set

=cut

sub destroy_set {
    my $self = shift;
    my $attr = shift;

    my $t = tied $self->{$attr};
    return if (defined $t and $t =~ m,Tangram::CollOnDemand,);
    if (ref $self->{$attr} eq "Set::Object") {
	$self->{$attr}->clear;
    }
    delete $self->{$attr};
}

=item destroy_hash

empties a hash

=cut

sub destroy_hash {
    my $self = shift;
    my $attr = shift;
    my $t = tied $self->{$attr};
    %{$self->{$attr}} = ()
	unless (defined $t and $t =~ m,Tangram::CollOnDemand,);
    delete $self->{$attr};
}

=item destroy_ref

destroys a reference.

=cut

sub destroy_ref {
    my $self = shift;
    delete $self->{shift};
}

=back

=item parse_X ($attribute, { schema option })

Parses the schema option field, and returns one or two closures that
act as a check_X and a destroy_X function for the attribute.

This is currently a very ugly hack, parsing the SQL type definition of
an object.  But it was bloody handy in my case for hacking this in
quickly.  This is probably unmanagably unportable across databases;
but send me bug reports on it anyway, and I'll try and make the
parsers work for as many databases as possible.

This perhaps should be replaced by primitives that go the other way,
building the SQL type definition from a more abstract definition of
the type.

Available functions:

=over

=item parse_string

parses SQL types of:

=over

=cut

use vars qw($quoted_part $sql_list);

$quoted_part = qr/(?: \"([^\"]+)\" | \'([^\']+)\' )/x;
$sql_list = qr/\(\s*
		  (
		      $quoted_part
		        (?:\s*,\s* $quoted_part )*
	          ) \s*\)/x;

sub parse_string {

    my $attribute = shift;
    my $option = shift;

    # simple case; return the check_string function.  We don't
    # need a destructor for a string so don't return one.
    if (!$option->{sql}) {
	return \&check_string;
    }

=item CHAR(N), VARCHAR(N)

closure checks length of string is less than N characters

=cut

    if ($option->{sql} =~ m/^\s*(?:var)?char\s*\(\s*(\d+)\s*\)/ix) {
	my $max_length = $1;
	return sub {
	    die "string too long for $attribute"
		if (length ${$_[0]} > $max_length);
	};

=item TINYBLOB, BLOB, LONGBLOB

checks max. length of string to be 255, 65535 or 16777215 chars
respectively.  Also works with "TEXT" instead of "BLOB"

=cut

    } elsif ($option->{sql} =~ m/^\s*(?:tiny|long|medium)?
				 (?:blob|text)/ix) {
	my $max_length = ($1 ? ($1 eq "tiny"?255:2**24 - 1)
			  : 2**16 - 1);
	return sub {
	    die "string too long for $attribute"
		if (length ${$_[0]} > $max_length);
	};

=item SET("members", "of", "set")

checks that the value passed is valid as a SQL set type, and that all
of the passed values are allowed to be a member of that set.

=cut

    } elsif (my ($members) = $option->{sql} =~
	     m/^\s*set\s*$sql_list/oi) {

	my %members;
	$members{lc($1 || $2)} = 1
	    while ( $members =~ m/\G[,\s]*$quoted_part/cog );

	return sub {
	    for my $x (split /\s*,\s*/, ${$_[0]}) {
		croak ("SQL set badly formed or invalid member $x "
		       ." (SET" . join(",", keys %members). ")")
		    if (not exists $members{lc($x)});
	    }
	};

=item ENUM("possible", "values")

checks that the value passed is one of the allowed values.

=cut

    } elsif (my ($values) = $option->{sql} =~
	     m/^\s*enum\s*$sql_list/oi ) {

	my %values;
	$values{lc($1 || $2)} = 1
	    while ( $values =~ m/\G[,\s]*$quoted_part/gc);

	return sub {
	    croak ("invalid enum value ${$_[0]} must be ("
		   . join(",", keys %values). ")")
		if (not exists $values{lc(${$_[0]})});
	}


    } else {
	die ("Please build support for your string SQL type in "
	     ."Class::Tangram (".$option->{sql}.")");
    }
}

=back

=back

=item import_schema($class)

Parses a tangram object schema, in "\$${class}::schema" to the
internal representation used to check types values by set().  Called
automatically on the first get(), set(), or new() for an object of a
given class.

This function updates Tangram schema option hashes, with the following
keys:

=over

=item check_func

supply/override the check_X function for this attribute.

=item destroy_func

supply/override the destroy_X function for this attribute

=back

See the SYNOPSIS section for an example of supplying a check_func in
an object schema.

=cut

sub import_schema($) {
    my $class = shift;

    # FIXME - expand the run time type information functions, then use
    # them for this function.

    eval {
	my ($fields, $bases, $abstract);
	{
	    no strict 'refs';
	    local $^W=0;
	    eval {
		$fields = ${"${class}::schema"}->{fields};
		$bases = ${"${class}::schema"}->{bases};
		$abstract = ${"${class}::schema"}->{abstract};
	    };
	    if ( !$fields and !$bases ) {
		# hack to pass the "empty inheritance" test
		my @stack = @{"${class}::ISA"};
		my %seen = map { $_ => 1 } $class, __PACKAGE__;
		$bases = [];
		while ( my $super = pop @stack ) {
		    if ( defined ${"${super}::schema"} ) {
			push @$bases, $super;
		    } else {
			push @stack, grep { !$seen{$_}++ }
			    @{"${super}::ISA"};
		    }
		}
		@$bases
		    or die ("No schema and no Class::Tangram "
			    ."superclass for $class; define "
			    ."${class}::schema!");
	    }
	}

	my $check_class = { };
	my $cleaners_class = { };
	my $init_defaults_class = { };
	my $types_class = { };
	my $defs_class = { };

	# if this is an abstract type, do not allow it to be
	# instantiated
	if ($abstract) {
	    $abstract{$class} = 1;
	}

	# If there are any base classes, import them first so that the
	# check, cleaners and init_defaults can be inherited
	if (defined $bases) {
	    (ref $bases eq "ARRAY")
		or die "bases not an array ref for $class";

	    # Note that the order of your bases is significant, that
	    # is if you are using multiple iheritance then the later
	    # classes override the earlier ones.
	    for my $super ( @$bases ) {
		import_schema $super unless (exists $check{$super});

		# copy each of the per-class configuration hashes to
		# this class as defaults.
		my ($k, $v);

		# FIXME - this repetition of code is getting silly :)
		$types_class->{$k} = $v
		    while (($k, $v) = each %{ $types{$super} } );
		$check_class->{$k} = $v
		    while (($k, $v) = each %{ $check{$super} } );
		$cleaners_class->{$k} = $v
		    while (($k, $v) = each %{ $cleaners{$super} } );
		$defs_class->{$k} = $v
		    while (($k, $v) = each %{ $attribute_options{$super} } );
		$init_defaults_class->{$k} = $v
		    while (($k, $v) = each %{ $init_defaults{$super} } );
	    }
	}

	# iterate over each of the *types* of fields (string, int, ref, etc.)
	while (my ($type, $v) = each %$fields) {
	    if (ref $v eq "ARRAY") {
		$v = { map { $_, undef } @$v };
	    }
	    my $def = $defaults{$type};

	    # iterate each of the *attributes* of a particular type
	    while (my ($attribute, $option) = each %$v) {
		$types_class->{$attribute} = $type;

		# ----- check_X functions ----
		$defs_class->{$attribute} = $option || {};

		if (ref $option eq "HASH" and $option->{check_func}) {
		    # user-supplied check_X function
		    $check_class->{$attribute} =
			$option->{check_func};

		} else {
		    if (not defined $def) {
			die "No default check function for type $type";
		    }

		    # check for a type-specific option hash parser
		    if ($def->{parse}) {
			my ($check, $destroy) =
			    $def->{parse}->($attribute, $option);

			$check_class->{$attribute} = $check;
			$cleaners_class->{$attribute} = $destroy
			    if (defined $destroy);

		    } else {
			# use the default for this type
			$check_class->{$attribute} = $def->{check};
		    }
		}

		# ----- destroy_X functions
		if (ref $option eq "HASH" and $option->{destroy_func}) {
		    # user-supplied destroy_X function
		    $cleaners_class->{$attribute} =
			$option->{destroy_func};
		} else {
		    if ($def->{destroy}) {
			# use the default for this type
			$cleaners_class->{$attribute} =
			    $def->{destroy};
		    }
		}

		# ----- init_default functions
		# create empty Set::Object containers as necessary
		if ($type =~ m/^i?set$/) {
		    $init_defaults_class->{$attribute} =
			sub { Set::Object->new() };
		}
		if (ref $option eq "HASH" and $option->{init_default}) {
		    $init_defaults_class->{$attribute} =
			$option->{init_default};
		}
	    }
	}
	$types{$class} = $types_class;
	$check{$class} = $check_class;
	$cleaners{$class} = $cleaners_class;
	$init_defaults{$class} = $init_defaults_class;
	$attribute_options{$class} = $defs_class;
    };

    $@ && die "$@ while trying to import schema for $class";
}


=item $instance->quickdump

Quickly show the blessed hash of an object, without descending into
it.  Primarily useful when you have a large interconnected graph of
objects so don't want to use the B<x> command within the debugger.
It also doesn't have the side effect of auto-vivifying members.

This function returns a string, suitable for print()ing.  It does not
currently escape unprintable characters.

=cut

sub quickdump($) {
    my $self = shift;

    my $r = "REF ". (ref $self). "\n";
    for my $k (sort keys %$self) {
	$r .= ("   $k => "
	       . (
		  tied $self->{$k}
		  || ( ref $self->{$k}
		       ? $self->{$k}
		       : "'".$self->{$k}."'" )
		 )
	       . "\n");
    }
    return $r;
}


=item $instance->DESTROY

This function ensures that all of your attributes have their
destructors called.  It calls the destroy_X function for attributes
that have it defined, if that attribute exists in the instance that we
are destroying.  It calls the destroy_X functions as destroy_X($self,
$k)

=cut

sub DESTROY($) {
    my $self = shift;

    my $class = ref $self;

    # if no cleaners are known for this class, it hasn't been imported
    # yet.  Don't call import_schema, that would be a bad idea in a
    # destructor.
    exists $cleaners{$class} or return;

    # for every attribute that is defined, and has a cleaner function,
    # call the cleaner function.
    for my $k (keys %$self) {
	if (defined $cleaners{$class}->{$k} and exists $self->{$k}) {
	    $cleaners{$class}->{$k}->($self, $k);
	}
    }
    $self->{_DESTROYED} = 1;
}

=item $instance->clear_refs

This clears all references from this object, ie exactly what DESTROY
normally does, but calling an object's destructor method directly is
bad form.  Also, this function has no qualms with loading the class'
schema with import_schema() as needed.

This is useful for breaking circular references, if you know you are
no longer going to be using an object then you can call this method,
which in many cases will end up cleaning up most of the objects you
want to get rid of.

However, it still won't do anything about Tangram's internal reference
to the object, which must still be explicitly unlinked with the
Tangram::Storage->unload method.

=cut

sub clear_refs($) {
    my $self = shift;
    my $class = ref $self;

    exists $cleaners{$class} or import_schema($class);

    # break all ref's, sets, arrays
    for my $k (keys %$self) {
	if (defined $cleaners{$class}->{$k} and exists $self->{$k}) {
	    $cleaners{$class}->{$k}->($self, $k);
	}
    }
    $self->{_NOREFS} = 1;
}

=back

=head1 Run-time type information

It is possible to access the data structures that Class::Tangram uses
internally to verify attributes, create objects and so on.

Class::Tangram keeps six internal hashes:

=over

=item %types

$types{$class}->{$attribute} is the tangram type of each attribute, ie
"ref", "iset", etc.  See L<Tangram::Type>.

=item %attribute_options

$attribute_options{$class}->{$attribute} is the options hash for a
given attribute.

=item %check

$check{$class}->{$attribute} is a function that will be passed a
reference to the value to be checked and either throw an exception
(die) or return true.

=item %cleaners

$attribute_options{$class}->{$attribute} is a reference to a
destructor function for that attribute.  It is called as an object
method on the object being destroyed, and should ensure that any
circular references that this object is involved in get cleared.

=item %abstract

$abstract->{$class} is set if the class is abstract

=item %init_defaults

$init_defaults{$class}->{$attribute} represents what an attribute is
set to automatically if it is not specified when an object is
created. If this is a scalar value, the attribute is set to the
value. If it is a function, then that function is called (as a method)
and should return the value to be placed into that attribute.  If it
is a hash ref or an array ref, then that structure is COPIED in to the
new object.  If you don't want that, you can do something like this:

   [...]
    flat_hash => {
        attribute => {
            init_default => sub { { key => "value" } },
        },
    },
   [...]

Now, every new object will share the same hash for that attribute.

=back

There are currently four functions that allow you to access parts of
this information.

=over

=item attribute_options($class)

Returns a hash ref to a data structure from attribute names to the
option hash for that attribute.

=cut

sub attribute_options($) {
    my $class = shift;
    return $attribute_options{$class};
}

=item attribute_types($class)

Returns a hash ref from attribute names to the tangram type for that
attribute.

=cut

sub attribute_types($) {
    my $class = shift;
    return $types{$class};
}

=item known_classes

This function returns a list of all the classes that have had their
object schema imported by Class::Tangram.

=cut

sub known_classes {
    return keys %types;
}

=item is_abstract($class)

This function returns true if the supplied class is abstract.

=cut

sub is_abstract {
    my $class = shift;
    $class eq "Class::Tangram" && ($class = shift);

    exists $cleaners{$class} or import_schema($class);
}

=back

=item Class->set_init_default(attribute => $value);

Sets the default value on an attribute for newly created "Class"
objects, as if it had been declared with init_default.  Can be called
as a class or an instance method.

=cut

sub set_init_default {
    my $invocant = shift;
    my $class = ref $invocant || $invocant;

    exists $init_defaults{$class} or import_schema($class);

    while ( my ($attribute, $value) = splice @_, 0, 2) {
	$init_defaults{$class}->{$attribute} = $value;
    }
}

package Tangram::Transient;

BEGIN {
    eval "use base qw(Tangram::Type)";
    if ( $@ ) {
	# no tangram
    } else {
	$Tangram::Schema::TYPES{transient} =  bless {}, __PACKAGE__;
    }
}

sub coldefs { }


=head1 SEE ALSO

L<Tangram::Schema>

B<A guided tour of Tangram, by Sound Object Logic.>

 http://www.soundobjectlogic.com/tangram/guided_tour/fs.html

=head1 BUGS/TODO

More AUTOLOAD methods, in particular for container types such as
array, hash, etc.  [on second thoughts, this might not be necessary.
You can always return a tied hash].

There should be more functions for breaking loops; in particular, a
standard function called drop_refs($obj), which replaces references to
$obj with the appropriate Tangram::RefOnDemand so that an object can
be unloaded via Tangram::Storage->unload() and actually have a hope of
being reclaimed.  Another function that would be handy would be a
deep "mark" operation for mark & sweep garbage collection.

Need to think about writing some functions using Inline for speed.

Allow init_default to be set in import function?

non-persistent attributes

=head1 AUTHOR

Sam Vilain, <sam@vilain.net>

=cut

69;
