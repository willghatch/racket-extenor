# Extenor

This is a library for a kind of Racket record object that can be dynamically extended (including with struct-type-properties), keep its extensions when being updated with standard functions (unlike normal Racket structs), and can be introspected to some degree.

So far it's just a prototype, but I intend to work it into something useful, in particular for Rash, and this readme is just an idea roadmap for now.

The key thing I *really* want in Racket, and that this project tries to be, is something that supports struct-type-properties that is robustly extensible and mixin-able, and that can do it at the instance level rather than at the type level.
Structs fail at this because when a struct subtype is functionally updated by any function that doesn't know about the subtype, the extensions are lost on the copy.
Racket classes have mixin functionality, but they don't seem to support struct-type-properties (if I'm wrong about this, please let me know!).

Aside from that first overarching goal, I also want to make something that is a bit of a mix of what I like about nominal records and dictionary-style objects.
Whether this is actually a good idea I have yet to see...  But if nothing else it seems like a good enough idea that I'm going to pursue it to some degree.
Powershell is some inspiration for what I want to do, since Powershell objects have the Add-Member method.  It let's you extend objects with new fields in the middle of a pipeline.  Though I'm not sure how useful that really is in practice.

Additionally I want to support some kind of serialization/deserialization, to be able to use these objects in pipelines that include subprocesses -- eg. send these objects through `grep` and get them back out.
This is at odds with support for struct-type-properties and encapsulation that I would like to support.
I think I may have some kind of serialization function that adds some kind of ID to each record's visible, named, dictionary-style fields and generates a unique deserializer that recognizes those IDs to generate (potentially modified versions of) the objects that were serialized with their original non-serializable fields and properties.

My hand-wavy vision is to have commands for use in Rash that you can pipe around and augment (with extra data OR support for new interfaces via properties).  Maybe do some kind of useful merges on (eg. like table merges in SQL but with shell data -- eg. maybe you merge the data you would get from `ps` and the data you would get from `ls` about processes and the executable files that back them).  And of course have more functionality be genericized so that these objects can be shoe-horned into more roles (eg. file operations take `path-string?`s, but why isn't there a `prop:path`?  This isn't really addressed by this library, but I'll probably have more motivation to go around making these generic properties once I have the kind of dynamic object that I want to make leverage these generics.).  I want these objects to be easily sortable, filterable, etc in generic ways using field names, but also support struct-like accessors/mutators and some amount of encapsulation.  I want to be able to eg. have generic ways of viewing lists of these objects as a table with easy-to-customize rules about what columns are displayed and in what order.  I've always intended Rash to grow more useful and powerful by adding realistic, useful support for Racket functions as command and pipelines full of Racket data by having a suite of useful commands written as Racket functions (eg. like how Powershell is really useful on Windows because there are lots of “cmdlets” that work on Windows and provide useful objects instead of strings for system administration tasks, while Powershell is considerably less useful on Unix because basically all of the standard cmdlets are Windows-only!).  But every time I think about writing a set of commands in Racket I end up thinking about how common things in Racket aren't sufficiently generic and how I want the results of these commands to be expressed in robustly extensible objects that can fulfill various useful interfaces.  So I think once I make this library useful I'll start making good on that intention for Rash.


# Design decisions made

An `extenor` is an “EXTEnsible NOminal Record”.
Extenors are purely functional, but may contain mutable values in their fields (eg. a box).
An extenor is made of various `extenorcl`s (Components).
An extenorcl is like a struct-type -- it has named fields, properties, and optionally a “guard” that interposes on construction and update.
An extenor is essentially an instantiation of a combination of extenorcls, IE extenorcls are the (first-class) types and extenors are instances of (some mix of) those types.
For now, at least, you can't make subtypes of extenorcls, but extenors form a subtype lattice according to which extenorcls they include (When I say “subtype” here, of course, I mean what one means when discussing types for a dynamically/un(i)typed language.  For the moment I am not delving into the static typing of these purposely very dynamic objects I want to use in my shell.  But I note that they have some relation to row types.  Though in principle I'm interested in a statically typed Rash-like shell, or adding gradual typing to Rash.  Though it's not my immediate priority.  What does it say about myself and/or the state of PL discourse that I feel the need to proactively explain my usage of the word “type” and this little project's relationship to the broader PL research world?).
When defining extenorcls, you get predicate, accessor, and mutator functions which apply to extenors.
When (functional) updates occur, all the fields and properties of all the extenorcls in the extenor are copied.
When an extenorcl with a guard is added to an extenor or has a field updated within an extenor, the guard is applied to the (new) fields relevant to that extenor.
The guard has a chance to raise an error or replace those fields, but it can't interpose on the fields of other extenorcls.
Extenors also act like a dictionary (though not conforming to Racket's `dict?` interface unless an attached extenorcl provides that functionality via `prop:dict`, which may be done in various different ways).
Extenorcl fields may be visible or hidden.
Visible fields are readable and (functionally) updatable via the dictionary interface, while hidden fields are only readable and updatable via the getter/setter functions provided at the time of extenorcl construction.
Extenorcls are in conflict if they share the same visible fields or struct type properties (or if they are the same extenorcl according to `eq?`).
Interned symbols are allowed as convenient, on-the-fly, single-field extenorcls.
Symbol extenorcls rely on the dictionary interface, and don't supply accessor and mutator functions.


# TODO items:

* What semantics do I want when there are field conflicts within components?  The simplest and most sensible is to error, which I think should at least be the default.  But sometimes does it may make sense to shadow names while leaving the old component intact (and accessible by accessor)?  These are classic multiple inheritance problems, but I frankly haven't thought a lot about these things.  I should read up on it (including reading what Racket class mixins do and why).
* What APIs do I want?
  - I want to allow merging objects, which runs into the field conflict above.  Maybe I have something like `merge-disjoint` that only merges extenors with no conflicts as well as `merge-eq?` and `merge-equal?` where merges are allowed when conflicting fields are equal in both objects.
  - I want to allow removing components or fields, but relatedly what should happen when removing a “field” that is part of a more complicated component?  Probably I would remove the entire component, but that is certainly different from removing fields!
  - Should I allow the components to be listed?  Is this the same as making the constructor for each component always visible and accessible?  Does this destroy encapsulation?  Components can be queried and inspected if you have the predicate and accessor functions for them.  How much introspection or encapsulation do I want?
  - Should I expose the difference between symbols as degenerate single-field visible components and “proper” components that may contain multiple fields, hidden fields, and properties?
* How do I really want (limited) serialization/deserialization to work?
* What representation do I want?  Is there a way I can make it fast for each of:
  - adding a new component (set of fields/properties) or single field (degenerate case of component)
  - updating a field by update function (struct-like)
  - updating a field by name (dictionary-like)
  - accessing a field by accessor function (struct-like)
  - accessing a field by name (dictionary-like)
  - iteration as a collection
* I can happily use struct-type-properties with extenors, but not generics.  Generics only have static APIs (eg. you can add generic support to struct-types defined with the `struct` macro but not constructed dynamically `make-struct-type` procedure!), so I have no obvious way to support them.  But using generics instead of properties is what the documentation recommends!  I should either find a way to support generics via properties (because that's what they compile to) or look into adding dynamic APIs for dealing with generics to Racket.


The major motivations for wanting to remove or shadow extenorcls and their fields are:

* Replacing a struct-type-property.  Eg. a function that constructs and returns extenors will be immediately more useful if it adds useful properties, such as `prop:custom-write`, `prop:dict`, etc.  But without a removal capability, this would block better or more task-appropriate versions of these properties from being used.  Also having these properties on by default would block merge operations on extenors that share a common property like `prop:custom-write`.
* Overriding some field that's used in a duck-type way by functions related to one extenorcl (IE an extenorcl that assumes more about the structure of the host extenor than what is safe based on that extenorcl alone) but is used in a conflicting way by another extenorcl.  This is a bit of a mess, really, but I'm not immediately sure what to do about it in a principled way that doesn't drag me into having static types (and an advanced, fancy type system at that).  But I do want extenorcls to be easily mix-in-able, and it is tempting to have some capabilities packaged as extenorcls that require other dependencies to be injected by another party (hopefully with documented contracts).
