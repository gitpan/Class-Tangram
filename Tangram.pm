package Class::Tangram;

# Copyright (c) 2001, 2002 Sam Vilain.  All right reserved.  This file
# is licensed under the terms of the Perl Artistic license.

=head1 NAME

Class::Tangram - create constructors, accessor and update methods for
objects from a Tangram-compatible object specification.

=head1 SYNOPSIS

 package MyObject;

 use base qw(Class::Tangram);

 our $fields => { int    => [ qw(foo bar) ],
                  string => [ qw(baz quux) ] };

 package main;

 my $object = MyObject->new(foo => 2, baz => "hello");

 print $object->baz();            # prints "hello"

 $object->set_quux("Something");

 $object->set_foo("Something");   # dies - not an integer

=head1 DESCRIPTION

Class::Tangram is a tool for defining objects attributes.  Simply
define your object's fields/attributes using the same syntax
introduced in _A Guided Tour of Tangram_, and you get objects that
work As You'd Expect(tm).

Class::Tangram has no dependancy upon Tangram, and vice versa.
Neither requires anything special of your objects, nor do they insert
any special fields into your objects.  This is a very important
feature with innumerable benefits, and few (if any) other object
persistence tools have this feature.

So, fluff aside, let's run through how you use Class::Tangram to make
objects.

First, you decide upon the attributes your object is going to have.
You might do this using UML, or you might pick an existing database
table and declare each column to be an attribute (you can leave out
"id"; that one is implicit).

Your object should use Class::Tangram as a base class;

  use base qw(Class::Tangram)

or for older versions of perl:

  use Class::Tangram;
  use vars qw(@ISA);
  @ISA = qw(Class::Tangram)

You should then define a C<$fields> variable in the scope of the
package, that is a B<hash> from attribute B<types> (see
L<Tangram::Type>) to either an B<array> of B<attribute names>, or
another B<hash> from B<attribute names> to B<options hashes> (or
C<undef>).  The layout of this structure coincides with the C<fields>
portion of a tangram schema (see L<Tangram::Schema>).  Note: the term
`schema' is used frequently to refer to the C<$fields> structure.

For example,

 package MyObject;
 use base qw(Class::Tangram);

 our $fields = {
     int => {
         juiciness => undef,
         segments => {
             # this code reference is called when this
             # attribute is set, to check the value is
             # OK
             check_func => sub {
                 die "too many segments"
                     if (${(shift)} > 30);
             },
             # the default for this attribute.
             init_default => 7,
         },
     },
     ref => {
        grower => undef,
     },

     # 'required' attributes - insist that these fields are
     # set, both with constructor and set()/set_X methods
     string => {
         # true: 'type' must have non-empty value (for
         # strings) or be logically true (for other types)
	 type => { required => 1 },

	 # false: 'tag' must be defined but may be empty
	 tag => { required => '' },
     },

     # fields allowed by Class::Tangram but not ever
     # stored by Tangram - no type checking by default
     transient => [ qw(_tangible) ],
 };

It is of critical importance to your sanity that you understand how
anonymous hashes and anonymous arrays work in Perl.  Some additional
features are used above that have not yet been introduced, but you
should be able to look at the above data structure and see that it
satisfies the conditions stated in the paragraph before it.  If it is
hazy, I recommend reading L<perlref> or L<perlreftut>.

When the schema for the object is first imported (see L<Schema
import>), Class::Tangram defines accessor functions for each of the
attributes defined in the schema.  These accessor functions are then
available as C<$object-E<gt>function> on created objects.  By virtue
of inheritance, various other methods are available.

From Class::Tangram 1.12 onwards, no use of perl's C<AUTOLOAD>
functionality is used.

=cut

use strict;
use Carp qw(croak cluck);

use vars qw($VERSION %defaults);

use Set::Object;

$VERSION = 1.13;

%defaults = (
 int         => { check_func   => \&check_int },
 real        => { check_func   => \&check_real },
 string      => { parse        => \&parse_string },
 ref         => { check_func   => \&check_obj,
		  destroy_func => \&destroy_ref },
 array       => { check_func   => \&check_array,
		  destroy_func => \&destroy_array },
 iarray      => { check_func   => \&check_array,
		  destroy_func => \&destroy_array },
 flat_array  => { check_func   => \&check_flat_array },
 set         => { check_func   => \&check_set,
		  destroy_func => \&destroy_set,
		  init_default => sub { Set::Object->new() } },
 iset        => { check_func   => \&check_set,
		  destroy_func => \&destroy_set,
		  init_default => sub { Set::Object->new() } },
 dmdatetime  => { check_func   => \&check_dmdatetime },
 rawdatetime => { check_func   => \&check_rawdatetime },
 rawdate     => { check_func   => \&check_rawdate },
 rawtime     => { check_func   => \&check_rawtime },
 flat_hash   => { check_func   => \&check_flat_hash },
 transient   => { check_func   => \&check_nothing },
 hash        => { check_func   => \&check_hash,
		  destroy_func => \&destroy_hash,
		  get_func     => \&get_hash },
 perl_dump   => { check_func   => \&check_nothing }
);

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

# $required_attributes{$class}->{$attribute} records which attributes
# are required... used only by new() at present.
my (%required_attributes);

# if a class is abstract, complain if one is constructed.
my (%abstract);

=head1 METHODS

The following methods are available for all Class::Tangram objects

=head2 Constructor

A Constructor is a method that returns a new instance of an object.

=over 4

=item Class-E<gt>new (attribute1 =E<gt> value, attribute2 =E<gt> value)

Sets up a new object of type C<Class>, with attributes set to the
values supplied.

Can also be used as an object method (normal use is as a "class
method"), in which case it returns a B<copy> of the object, without
any deep copying.

=cut

sub new ($@)
{
    my $invocant = shift;
    my $class = ref $invocant || $invocant;

    (my @values, @_) = @_;

    # Setup the object
    my $self = { };
    bless $self, $class;

    # auto-load schema as necessary
    exists $types{$class} or import_schema($class);

    croak "Attempt to instantiate an abstract type"
	if ($abstract{$class});

    if (ref $invocant)
    {
	# The copy constructor; this could be better :)
	# this has the side effect of much auto-vivification.
	%$self = %$invocant;
	$self->set (@values); # override with @values
    }
    else
    {
	$self->set (@values); # start with @values
    }

    $self->_fill_init_default();
    $self->_check_required();

    return $self;

}

sub _fill_init_default {
    my $self = shift;
    my $class = ref $self or die "_fill_init_default usage error";

    # fill in fields that have defaults
    while ( my ($attribute, $default) =
	    each %{$init_defaults{$class}} ) {

	next if (exists $self->{$attribute});

	my $setter = "set_$attribute";
	if (ref $default eq "CODE") {
	    # sub { }, attribute gets return value
	    $self->$setter( $default->($self) );

	} elsif (ref $default eq "HASH") {
	    # hash ref, copy hash
	    $self->$setter( { %{ $default } } );

	} elsif (ref $default eq "ARRAY") {
	    # array ref, copy array
	    $self->$setter( [ @{ $default } ] );

	} else {
	    # something else, an object or a scalar
	    $self->$setter($default);
	}
    }
}

sub _check_required {
    my $self = shift;
    my $class = ref $self;

    # make sure field is not undef if 'required' option is set
    if (my $required = $required_attributes{$class}) {

	# find the immediate caller outside of this package
	my $i = 0;
	$i++ while UNIVERSAL::isa($self, scalar(caller($i))||";->");

	# give Tangram some lenience - it is exempt from the effects
	# of the "required" option
	unless ( caller($i) =~ m/^Tangram::/ ) {
	    my @missing;
	    while ( my ($attribute, $value) = each %$required ) {
		push(@missing, $attribute)
		    if ! exists $self->{$attribute};
	    }
	    croak("object missing required attribute(s): "
		  .join(', ',@missing).'.') if @missing;
	}
    }
}

=back

=head2 Accessing & Setting Attributes

=over

=item $instance->set(attribute => $value, ...)

Sets the attributes of the given instance to the given values.  croaks
if there is a problem with the values.

This function simply calls C<$instance-E<gt>set_attribute($value)> for
each of the C<attribute =E<gt> $value> pairs passed to it.

=cut

sub set {
    my $self = shift;

    # yes, this is a lot to do.  yes, it's slow.  But I'm fairly
    # certain that this could be handled efficiently if it were to be
    # moved inside the Perl interpreter or an XS module
    UNIVERSAL::isa($self, "Class::Tangram") or croak "type mismatch";
    my $class = ref $self;
    exists $check{$class} or import_schema($class);
    croak "set must be called with an even number of arguments"
	if (scalar(@_) & 1);

    while (my ($name, $value) = splice @_, 0, 2) {

	my $setter = "set_".$name;

	croak "attempt to set an illegal field $name in a $class"
	    unless $self->can($setter);

	$self->$setter($value);
    }
}

=item $instance->get("attribute")

Gets the value of C<$attribute>.  This simply calls
C<$instance-E<gt>get_attribute>.  If multiple attributes are listed,
then a list of the attribute values is returned in order.  Note that
you get back the results of the scalar context C<get_attribute> call
in this case.

=cut

sub get {
    my $self = shift;
    croak "get what?" unless @_;
    UNIVERSAL::isa($self, "Class::Tangram") or croak "type mismatch";

    my $class = ref $self;
    exists $check{$class} or import_schema($class);

    my $multiget = (scalar(@_) != 1);

    my @return;
    while ( my $field = shift ) {
	my $getter = "get_".$field;
	croak "attempt to read an illegal field $field in a $class"
	    unless $self->can($getter);

	if ( $multiget ) {
	    push @return, scalar($self->$getter());
	} else {
	    return $self->$getter();
	}
    }

    return @return;
}

=item $instance->attribute($value)

If C<$value> is not given, then
this is equivalent to C<$instance-E<gt>get_attribute>

If C<$value> is given, then this is equivalent to
C<$instance-E<gt>set_attribute($value)>.  This usage issues a warning
if warnings are on; you should change your code to use the
set_attribute syntax for better readability.  OO veterans will tell
you that for maintainability object method names should always be a
verb.

=item $instance->get_attribute

=item $instance->set_attribute($value)

The normative way of getting and setting attributes.  If you wish to
override the behaviour of an object when getting or setting an
attribute, override these functions.  They will be called when you use
C<$instance-E<gt>attribute>, C<$instance-E<gt>get()>, constructors,
etc.

=item $instance->attribute_includes(@objects)

=item $instance->attribute_insert(@objects)

=item $instance->attribute_size

=item $instance->attribute_clear

=item $instance->attribute_remove(@objects)

This suite of functions applies to attributes that are sets (C<iset>
or C<set>).  It could in theory also apply generally to all
collections - ie also arrays (C<iarray> or C<array>), and hashes
(C<hash>, C<ihash>).  This will be implemented subject to user demand.

=back

B<Note:> The above functions can be overridden, but they may not be
called with the C<$self-E<gt>SUPER::> superclass chaining method.
This is because they are not defined within the scope of
Class::Tangram, only your package.

=cut

=head1 ATTRIBUTE TYPE CHECKING

Class::Tangram provides type checking of attributes when attributes
are set - either using the default C<set_attribute> functions, or
created via the C<new> constructor.

The checking has default behaviour for each type of attribute (see
L<Default Type Checking>), and can be extended arbitrarily via a
per-attribute C<check_func>, described below.  Critical attributes can
be marked as such with the C<required> flag.

The specification of this type checking is placed in the class schema,
in the per-attribute B<options hash>.  This is a Class::Tangram
extension to the Tangram schema structure.

=over

=item check_func

A function that is called with a B<reference> to the new value in
C<$_[0]>.  It should call C<die()> if the value is bad.  Note that
this check_func will never be passed an undefined value; this is
covered by the "required" option, below.

In the example schema (above), the attribute C<segments> has a
C<check_func> that prevents setting the value to anything greater than
30.  Note that it does not prevent you from setting the value to
something that is not an integer; if you define a C<check_func>, it
replaces the default.

=item required

If this option is set to a true value, then the attribute must be set
to a true value to pass type checking.  For string attributes, this
means that the string must be defined and non-empty (so "0" is true).
For other attribute types, the normal Perl definition of logical truth
is used.

If the required option is defined but logically false, (ie "" or 0),
then the attribute must also be defined, but may be set to a logically
false value.

If the required option is undefined, then the attribute may be set to
an undefined value.

For integration with tangram, the C<new()> function has a special
hack; if it is being invoked from within Tangram, then the required
test is skipped.

=back

=head2 Other per-attribute options

Any of the following options may be inserted into the per-attribute
B<options hash>:

=over

=item init_default

This value specifies the default value of the attribute when
it is created with C<new()>.  It is a scalar value, it is
copied to the fresh object.  If it is a code reference, that
code reference is called and its return value inserted into
the attribute.  If it is an ARRAY or HASH reference, then
that array or hash is COPIED into the attribute.

=item destroy_func

If anything special needs to happen to this attribute before the
object is destroyed (or when someone calls
C<$object-E<gt>clear_refs()>), then define this.  It is called as
C<$sub-E<gt>($object, "attribute")>.

=back


=over

=item check_X (\$value)

This series of internal functions are built-in C<check_func> functions
defined for all of the standard Tangram attribute types.

=over

=item check_string

checks that the supplied value is less than 255 characters long.

=cut

sub check_string {
    croak "string too long"
	if (length ${$_[0]} > 255);
}

=item check_int

checks that the value is a (possibly signed) integer

=cut

my $int_re = qr/^-?\d+$/;
sub check_int {
    croak "not an integer"
	if (${$_[0]} !~ m/$int_re/o);
}

=item check_real

checks that the value is a real number, by stringifying it and
matching it against (C<m/^-?\d*(\.\d*)?(e-?\d*)?$/>).  Inefficient?
Yes.  Patches welcome.

=cut

my $real_re = qr/^-?\d*(\.\d*)?(e-?\d*)?$/;
sub check_real {
    croak "not a real number"
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

=cut

sub check_nothing { }

=back

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
		if (${$_[0]} and length ${$_[0]} > $max_length);
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

=back

=head2 Quick Object Dumping and Destruction

=over

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

=head1 FUNCTIONS

The following functions are not intended to be called as object
methods.

=head2 Schema Import

 our $fields = { int => [ qw(foo bar) ],
                 string => [ qw(baz quux) ] };

 # Version 1.115 and below compatibility:
 our $schema = {
    fields => { int => [ qw(foo bar) ],
                string => [ qw(baz quux) ] }
    };

=over

=item Class::Tangram::import_schema($class)

Parses a tangram object field list, in C<${"${class}::fields"}> (or
C<${"${class}::schema"}-E<gt>{fields}> to the internal type information
hashes.  It will also define all of the attribute accessor and update
methods in the C<$class> package.

Note that calling this function twice for the same class is not
tested and may produce arbitrary results.  Patches welcome.

=cut

sub import_schema($) {    # Damn this function is long
    my $class = shift;

    eval {
	my ($fields, $bases, $abstract);
	{

	    # Here, we go hunting around for their defined schema and
	    # options
	    no strict 'refs';
	    local $^W=0;
	    eval {
		$fields = (${"${class}::fields"} ||
			   ${"${class}::schema"}->{fields});
		$abstract = (${"${class}::abstract"} ||
			     ${"${class}::schema"}->{abstract});
		$bases = ${"${class}::schema"}->{bases};
	    };
	    if ( my @stack = @{"${class}::ISA"}) {
		# clean "bases" information from @ISA
		my %seen = map { $_ => 1 } $class, __PACKAGE__;
		$bases = [];
		while ( my $super = pop @stack ) {
		    if ( defined ${"${super}::schema"}
			 or defined ${"${super}::fields"} ) {
			push @$bases, $super;
		    } else {
			push @stack, grep { !$seen{$_}++ }
			    @{"${super}::ISA"};
		    }
		}
		if ( !$fields and !@$bases ) {
		    die ("No schema and no Class::Tangram "
			 ."superclass for $class; define "
			 ."${class}::fields!");
		}
	    }
	}

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
		$types{$class}->{$k} = $v
		    while (($k, $v) = each %{ $types{$super} } );
		$check{$class}->{$k} = $v
		    while (($k, $v) = each %{ $check{$super} } );
		$cleaners{$class}->{$k} = $v
		    while (($k, $v) = each %{ $cleaners{$super} } );
		$attribute_options{$class}->{$k} = $v
		    while (($k, $v) = each %{ $attribute_options{$super} } );
		$init_defaults{$class}->{$k} = $v
		    while (($k, $v) = each %{ $init_defaults{$super} } );
		$required_attributes{$class}->{$k} = $v
		    while (($k, $v) = each %{ $required_attributes{$super} } );
	    }
	}

	# iterate over each of the *types* of fields (string, int, ref, etc.)
	while (my ($type, $v) = each %$fields) {
	    if (ref $v eq "ARRAY") {
		$v = { map { $_, undef } @$v };
	    }
	    my $def = $defaults{$type};

	    # iterate each of the *attributes* of a particular type
	    while (my ($attribute, $options) = each %$v) {

		# this is what we are finding out about each attribute
		# $type is already set
		my ($default, $check_func, $required, $cleaner);
		# set defaults from what they give
		$options ||= {};
		if (ref $options eq "HASH" or
		    UNIVERSAL::isa($options, 'Tangram::Type')) {
		    ($check_func, $default, $required, $cleaner)
			= @{$options}{qw(check_func init_default
					 required destroy_func)};
		}

		# Fill their settings with info from defaults
		if (ref $def eq "HASH") {

		    # try to magically parse their options
		    if ( $def->{parse} and !($check_func and $cleaner) ) {
			my @a = $def->{parse}->($attribute, $options);
			$check_func ||= $a[0];
			$cleaner ||= $a[1];
		    }

		    # fall back to defaults for this class
		    $check_func ||= $def->{check_func};
		    $cleaner ||= $def->{destroy_func};
		    $default = $def->{init_default} unless defined $default;
		}

		# everything must be checked!
		die "No check function for ${class}\->$attribute (type $type)"
		    unless (ref $check_func eq "CODE");

		$types{$class}->{$attribute} = $type;
		$check{$class}->{$attribute} = $check_func;
		{
		    no strict "refs";
		    local ($^W) = 0;

		    # build an appropriate "get_attribute" method, and
		    # define other per-type methods
		    my ($get_closure, $set_closure);

		    # implement with closures for speed
		    if ( $type =~ m/i?set/ ) {

			# GET_$attribute (Set::Object)
			$get_closure = sub {
			    my $self = shift;
			    if ( !defined $self->{$attribute} ) {
				$self->{$attribute} = Set::Object->new();
			    }
			    my $set = $self->{$attribute};
			    ( wantarray ? $set->members : $set )
			};

			# and add a whole load of other functions too
			for my $set_method (qw(includes insert size clear
					       remove)) {

			    # ${attribute}_includes, etc
			    my $set_method_closure = sub {
				my $self = shift;
				$self->{$attribute} = Set::Object->new()
				    unless defined $self->{$attribute};
				return $self->{$attribute}->$set_method(@_);
			    };
			    *{$class."::${attribute}_$set_method"} =
				$set_method_closure unless 
				    (defined &{$class."::${attribute}_$set_method"});
			}

		    } elsif ( $type =~ m/i?array/ ) {

			# GET_$attribute (array)
			# allow array slices, and return whole array
			# in list context
			$get_closure = sub {
			    my $array = ($_[0]->{$attribute} ||= []);
			    shift;
			    if ( @_ ) {
				@{$array}[@_];
			    } else {
				( wantarray ? @{ $array } : $array )
			    }
			};

		    } elsif ( $type =~ m/i?hash/ ) {
			# GET_$attribute (hash)
			# allow hash slices, and return whole hash in
			# list context
			$get_closure = sub {
			    my $hash = ($_[0]->{$attribute} ||= {});
			    shift;
			    if ( @_ ) {
				@{$hash}{@_}
			    } else {
				( wantarray ? %{ $hash } : $hash );
			    }
			};
		    } else {
			# GET_$attribute (scalar)
			# return value only
			$get_closure = sub { $_[0]->{$attribute}; };
		    }

		    *{$class."::get_$attribute"} = $get_closure
			unless (defined &{$class."::get_$attribute"});

		    # SET_$attribute (all)
		    my $checkit = \$check{$class}->{$attribute};

		    # required hack for strings - duplicate the code
		    # to avoid the following string comparison for
		    # every set
		    if ( $type eq "string" ) {
			$set_closure = sub {
			    my $self = shift;
			    my $value = shift;
			    eval {
				if ( defined $value and length $value ) {
				    ${$checkit}->(\$value);
				} elsif ( $required ) {
				    die "value is required"
				} elsif ( defined $required ) {
				    die "value must be defined"
					unless defined $value;
				}
			    };
			    $@ && croak("value failed type check - ${class}->"
					."set_$attribute('$value') ($@)");
			    $self->{$attribute} = $value;
			};
		    } else {
			$set_closure = sub {
			    my $self = shift;
			    my $value = shift;
			    eval {
				if ( $value ) {
				    ${$checkit}->(\$value);
				} elsif ( $required ) {
				    die "value is required"
				} elsif ( defined $required ) {
				    die "value must be defined"
					unless defined $value;
				}
			    };
			    $@ && croak("value failed type check - ${class}->"
				    ."set_$attribute('$value') ($@)");
			    $self->{$attribute} = $value;
			};
		    }

		    # now export them into the caller's namespace
		    my ($getter, $setter)
			= ("get_$attribute", "set_$attribute");
		    *{$class."::$getter"} = $get_closure
			unless defined &{$class."::$getter"};
		    *{$class."::$setter"} = $set_closure
			unless defined &{$class."::$setter"};

		    *{$class."::$attribute"} = sub {
			my $self = shift;
			if ( @_ ) {
			    warn("The OO Police say change your call "
				 ."to ->set_$attribute") if ($^W);
			    #goto $set_closure;  # NO!  BAD!! :-)
			    return $self->$setter(@_);
			} else {
			    return $self->$getter(@_);
			    #goto $get_closure;
			}
		    } unless defined &{$class."::$attribute"};
		}

		$cleaners{$class}->{$attribute} = $cleaner
		    if (defined $cleaner);
		$init_defaults{$class}->{$attribute} = $default
		    if (defined $default);
		$required_attributes{$class}->{$attribute} = $required
		    if (defined $required);
		$attribute_options{$class}->{$attribute} =
		    ( $options || {} );
	    }
	}
    };

    $@ && die "$@ while trying to import schema for $class";
}

=back

=head2 Run-time type information

It is possible to access the data structures that Class::Tangram uses
internally to verify attributes, create objects and so on.

This should be considered a B<HIGHLY EXPERIMENTAL> interface to
B<INTERNALS> of Class::Tangram.

Class::Tangram keeps seven internal hashes:

=over

=item C<%types>

C<$types{$class}-E<gt>{$attribute}> is the tangram type of each attribute,
ie "ref", "iset", etc.  See L<Tangram::Type>.

=item C<%attribute_options>

C<$attribute_options{$class}-E<gt>{$attribute}> is the options hash
for a given attribute.

=item C<%required_attributes>

C<$required_attributes{$class}-E<gt>{$attribute}> is the 'required'
option setting for a given attribute.

=item C<%check>

C<$check{$class}-E<gt>{$attribute}> is a function that will be passed
a reference to the value to be checked and either throw an exception
(die) or return true.

=item C<%cleaners>

C<$attribute_options{$class}-E<gt>{$attribute}> is a reference to a
destructor function for that attribute.  It is called as an object
method on the object being destroyed, and should ensure that any
circular references that this object is involved in get cleared.

=item C<%abstract>

C<$abstract-E<gt>{$class}> is set if the class is abstract

=item C<%init_defaults>

C<$init_defaults{$class}-E<gt>{$attribute}> represents what an
attribute is set to automatically if it is not specified when an
object is created. If this is a scalar value, the attribute is set to
the value. If it is a function, then that function is called (as a
method) and should return the value to be placed into that attribute.
If it is a hash ref or an array ref, then that structure is COPIED in
to the new object.  If you don't want that, you can do something like
this:

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

=item Class::Tangram::attribute_options($class)

Returns a hash ref to a data structure from attribute names to the
option hash for that attribute.

=cut

sub attribute_options($) {
    my $class = shift;
    return $attribute_options{$class};
}

=item Class::Tangram::attribute_types($class)

Returns a hash ref from attribute names to the tangram type for that
attribute.

=cut

sub attribute_types($) {
    my $class = shift;
    return $types{$class};
}

=item Class::Tangram::required_attributes($class)

Returns a hash ref from attribute names to the 'required' option setting for
that attribute.  May also be called as a method, as in
C<$instance-E<gt>required_attributes>.

=cut

sub required_attributes($) {
    my $class = ref $_[0] || $_[0];
    return $required_attributes{$class};
}

=item Class::Tangram::init_defaults($class)

Returns a hash ref from attribute names to the default intial values for
that attribute.  May also be called as a method, as in
C<$instance-E<gt>init_defaults>.

=cut

sub init_defaults($) {
    my $class = ref $_[0] || $_[0];
    return $init_defaults{$class};
}

=item Class::Tangram::known_classes

This function returns a list of all the classes that have had their
object schema imported by Class::Tangram.

=cut

sub known_classes {
    return keys %types;
}

=item Class::Tangram::is_abstract($class)

This function returns true if the supplied class is abstract.

=cut

sub is_abstract {
    my $class = shift;
    $class eq "Class::Tangram" && ($class = shift);

    exists $cleaners{$class} or import_schema($class);
}

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

=back

=cut

# a little embedded package

package Tangram::Transient;

BEGIN {
    eval "use base qw(Tangram::Type)";
    if ( $@ ) {
	# no tangram
    } else {
	$Tangram::Schema::TYPES{transient} = bless {}, __PACKAGE__;
    }
}

sub coldefs { }

sub get_exporter { }
sub get_importer { }

sub get_import_cols {
#    print "Get_import_cols:" , Dumper \@_;
    return ();
}

=head1 SEE ALSO

L<Tangram::Schema>

B<A guided tour of Tangram, by Sound Object Logic.>

 http://www.soundobjectlogic.com/tangram/guided_tour/fs.html

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

This is Class::Tangram version 1.13.

=head1 BUGS/TODO

There should be more functions for breaking loops; in particular, a
standard function called C<drop_refs($obj)>, which replaces references
to $obj with the appropriate C<Tangram::RefOnDemand> object so that an
object can be unloaded via C<Tangram::Storage->unload()> and actually
have a hope of being reclaimed.  Another function that would be handy
would be a deep "mark" operation for manual mark & sweep garbage
collection.

Need to think about writing some functions using C<Inline> for speed.
One of these days...

Allow C<init_default> values to be set in a default import function?

ie

  use MyClassTangramObject -defaults => { foo => "bar" };

=head1 AUTHOR

Sam Vilain, <sam@vilain.net>

=head2 CREDITS

 # Some modifications
 # Copyright Å© 2001 Micro Sharp Technologies, Inc., Vancouver, WA, USA
 # Author: Karl M. Hegbloom <karlheg@microsharp.com>
 # Perl Artistic Licence.

Many thanks to Charles Owens and David Wheeler for their feedback,
ideas, patches and bug testing.

=cut

69;

__END__

 # From old SYNOPSIS, I decided it was too long.  A lot of
 # the information here needs to be re-integrated into the
 # POD.

 package Project;

 # here's where we build the individual object schemas into
 # a Tangram::Schema object, which the Tangram::Storage
 # class uses to know which tables and columns to find
 # objects.
 use Tangram::Schema;

 # TIMTOWTDI - this is the condensed manpage version :)
 my $dbschema = Tangram::Schema->new
     ({ classes =>
       [ 'Orange'   => { fields => $Orange::fields },
         'MyObject' => { fields => $MyObject::schema }, ]});

 sub schema { $dbschema };

 package main;

 # See Tangram::Relational for instructions on using
 # "deploy" to create the database this connects to.  You
 # only have to do this if you want to write the objects to
 # a database.
 use Tangram::Relational;
 my ($dsn, $u, $p);
 my $storage = Tangram::Relational->connect
                   (Project->schema, $dsn, $u, $p);

 # Create an orange
 my $orange = Orange->new(
			  juiciness => 8,
			  type => 'Florida',
			  tag => '',  # required
			 );

 # Store it
 $storage->insert($orange);

 # This is how you get values out of the objects
 my $juiciness = $orange->juiciness;

 # a "ref" must be set to a blessed object, any object
 my $grower = bless { name => "Joe" }, "Farmer";
 $orange->set_grower ($grower);

 # these are all illegal - type checking is fairly strict
 my $orange = eval { Orange->new; };         print $@;
 eval { $orange->set_juiciness ("Yum"); };   print $@;
 eval { $orange->set_segments (31); };       print $@;
 eval { $orange->set_grower ("Mr. Nice"); }; print $@;

 # Demonstrate some "required" functionality
 eval { $orange->set_type (''); };           print $@;
 eval { $orange->set_type (undef); };        print $@;
 eval { $orange->set_tag (undef); };         print $@;

 # this works too, but is slower
 $orange->get( "juiciness" );
 $orange->set( juiciness => 123,
	       segments  => 17 );

 # Re-configure init_default - make each new orange have a
 # random juiciness
 $orange->set_init_default( juiciness => sub { int(rand(45)) } );
