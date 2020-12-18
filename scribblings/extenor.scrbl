#lang scribble/manual
@title[#:tag "Extenor"]{Extenor}
@author+email["William Hatch" "william@hatch.uno"]

@(require
  (for-label
   "../main.rkt"
   racket/base
   )
  )

@defmodule[extenor]

@section{Guide}

The Extenor package is an experimental package providing a particular kind of extensible object.
ExteNoR stands for “Extensible Nominal Record”, and an extenor can be an instance of multiple extenorcs, or “Extensible Nominal Record Components”.
An extenorc is somewhat like a struct-type or a class, in that it defines a set of fields that an instance must have, as well as struct-type-properties.
Each extenorc comes with a predicate that is true for extenors that are instances of that extenorc, as well as setter and getter functions.
Each extenor is immutable, and using a setter or extending an extenor with a new extenorc are functional opetations that return a fresh extenor.

The main motivation behind this library is to have something like structs that are extensible while still being functionally updatable.
Racket structs may be subtyped, so you can extend any given struct with new fields and struct-type-properties.
However, if you ever use a functional setter of the original struct-type on your extended struct instance, the object returned will be of the non-extended struct-type!
Extenors keep their extensions when they are functionally updated by functions that don't know about their extensions.

However, extenors are also an experiment with some other ideas in the hopes of making them particularly useful in conjunction with the Rash language.
In particular, extenorc fields have “visibility”.
Hidden extenorc fields are more like struct fields -- they can only be accessed with accessor functions specific to the extenorc.
Visible extenorc fields may also be accessed and updated via @racket[get-extenor-field] and @racket[set-extenor-field].
Visible extenor fields are more duck-typable (or perhaps structurally-typable, my dear pedantic PL nerd friends).
The main motivation here is that you might have various shell utility functions that you use in pipelines that operate on standard field names, and you may add fields dynamically in a pipeline to make data from some system administration function/command work with some other command.
Thus extenors give you something that simultaneously has some of the properties of a dictionary and some of the properties of a nominal record, and in particular the capability to have struct-type-properties (and in fact different struct-type-properties on each instance).



TODO - good walkthrough of examples.


@section{Reference}

See @secref{stability}.

TODO - add examples for each.

@defproc[(extenor? [v any/c]) bool?]{
Predicate - is this an extenor?
}

@defthing[empty-extenor extenor?]{
The empty extenor.
}

@defproc[(add-extenorc [an-extenor extenor?] [an-extenorc extenorc?]
                       [field-val any/c] ...)
         extenor?]{
Add an extenorc to an extenor.
The @racket[field-vals] must appropriately match the fields of the extenorc.
}

@defproc[(get-extenor-field [an-extenor extenor?] [field-name symbol?]
                            [fallback any/c (λ () (error 'get-extenor-field))])
         any/c]{
Get a visible field out of an extenor.
Note that the @racket[field-name] must be an interned symbol.
The @racket[fallback] optional argument acts like with @racket[hash-ref], if it is a procedure it is executed with no arguments, but as a convenience non-procedures may be provided and returned as-is.
}

@defproc[(set-extenor-field [an-extenor extenor?]
                            [field-name symbol?]
                            [new-value any/c])
         extenor?]{
Non-destructively (functionally) update a visible field of an extenor.
IE get a new extenor with the field value replaced.
If the extenor previously had no visible field, this effectively adds a degenerate extenorc providing that visible field.
}

@defproc[(get-extenor-keys [an-extenor extenor?]) (listof symbol?)]{
Returns a list of keys for visible extenor fields.
}
@defproc[(get-extenor-struct-type-properties [an-extenor extenor?])
         (listof struct-type-property?)]{
Returns a list of struct-type-properties supported by the extenor.
}


@defproc[(remove-extenorc [the-extenor extenor?] [the-extenorc extenorc?])
         extenor?]{
Returns an extenor like @racket[the-extenor] but with @racket[the-extenorc] removed.
}

@defproc[(extenorc? [v any/c]) bool?]{
Predicate -- is this an extenorc?
}
@defproc[(extenorc-name [an-extenorc extenorc?]) any/c]{
Get the name of an extenorc.

@emph{Stability - less stable than the rest of this library!}
}

@defproc[(get-extenorc-struct-type-properties [the-extenorc extenorc?])
         (listof struct-type-property?)]{
Return a list of the struct-type-properties supported by @racket[the-extenorc].
}

@defproc[(make-extenorc [#:name name (or/c #f (and/c symbol? symbol-interned?)) #f]
                        [#:guard guard (or/c #f procedure?) #f]
                        [#:properties properties
                         (hash/c struct-type-property? any/c)
                         (hash)]
                        [field-name-spec
                         (listof (cons/c (or/c 'hidden 'visible)
                                         (and/c symbol? symbol-interned?)))]
                        ...)
         (list/c extenorc?
                 (-> any/c any/c)
                 (listof (-> extenor? any/c))
                 (listof (-> extenor? any/c extenor?)))]{
Unless you want to dynamically generate extenorcs, you probably want @racket[define-extenorc] instead.

Make an extenorc.
This returns a list containing:
@itemlist[
@item{An extenorc object.}
@item{A predicate for extenors containing the extenorc object.}
@item{A list of getter functions that operate on extenors containing the extenorc.}
@item{A list of setter functions that operate on extenors containing the extenorc.  Note that they are functional setters that return a new extenor with an updated field, not a mutational setter.  Extenors are all immutable, though they may have mutable values in their fields.}
]


The @racket[name] argument provides a name for the extenorc.
@emph{Stability - the name field is less stable than the rest of this library!}

The @racket[guard] can be a procedure that guards what values can be set in the extenorc (both via the setter returned from this function and from @racket[set-extenor-field]) .
The @racket[guard] must be a procedure that accepts a value for each field of the extenorc and must return a list of the same size.
Guards are run for @racket[add-extenorc], for the setter of any field from the extenorc, or when @racket[set-extenor-field] is used on a field from the extenorc.
In other words, it can arbitrarily interpose on field setting, though this is probably not advisable.
It can also raise an exception, which is probably the better thing to do when given input that is improper for the extenorc.
@emph{Stability - I may change the API for guards.  As it is the guard can't tell which field is being set.  I want guards to be able to access all fields, so they can guard fields that have invariants relying on each other, but I also want a guard to have easy access to check an individual field.}

Each @racket[field-name-spec] is a pair specifying field visibility and field name.
Each field name must be an interned symbol.
Field visibility is either @racket['hidden] or @racket['visible].
Hidden fields can be accessed with a getter and setter returned by @racket[make-extenorc], but not with @racket[get-extenor-field] or @racket[set-extenor-field].
While each hidden field within a single extenorc must have a unique name, different extenorcs can use the same hidden field name.
Visible fields can be accessed with @racket[get-extenor-field] and @racket[set-extenor-field] (though setting a field may trigger a guard).
Visible fields in one extenorc may conflict with another, so that an extenor can only have one of the two extenorcs.

TODO - extenorcs should also support generics.  However, at the time of writing generics have only a static interface.  Properties can be added to dynamically generated structs via @racket[make-struct], while generics can't be.  This is poor, and despite wording in many parts of the Racket docs, generics should not be preferred over struct-type-properties until they have a similar dynamic interface.
}

@defform[(define-extenorc name (field-spec ...) keyword-arg ...)
         #:grammar ([field-spec (code:line field-name)
                                (code:line [hidden field-name])
                                (code:line [visible field-name])]
                    [keyword-arg (code:line #:guard guard-expression)
                                 (code:line #:property prop-expr val-expr)]
                    )]{
Static definition form wrapping @racket[make-extenorc].
Note that the @racket[#:property] clause can be used multiple times to add multiple properties to the extenorc.

Here's an example:
@racketblock[
             (define-extenorc frog (weight [hidden poisonous?] [visible name])
               #:property prop:custom-write
               (λ (self port mode) (fprintf port "<Frog: name=~v>" (frog-name self)))
               #:property prop:procedure
               (λ (self . args)
                 (set-extenor-field self weight
                                    (apply + (frog-weight self) args))))
             ]

This would define:
@itemlist[
@item{@tt{frog} as an extenorc}
@item{@tt{frog?} as a predicate for extenors that contain the extenorc}
@item{@tt{frog-weight} as a getter for the visible @tt{weight} field}
@item{@tt{set-frog-weight} as a setter for the visible @tt{weight} field}
@item{@tt{frog-poisonous?} as a getter for the hidden @tt{poisonous?} field}
@item{@tt{set-frog-poisonous?} as a setter for the hidden @tt{poisonous?} field}
@item{@tt{frog-name} as a getter for the visible @tt{poisonous?} field}
@item{@tt{set-frog-name} as a setter for the visible @tt{poisonous?} field}
]


Then we could run:

@racketblock[
             (define arnold-the-frog
               (add-extenorc empty-extenor frog 25 #f "arnold"))
             (define fat-arnold (arnold-the-frog 5 10 50))
             (code:comment "Returns 90")
             (get-extenor-field fat-arnold 'weight)
             ]


}

@defproc[(make-prop-extenorc [some-property struct-type-property?] [prop-val any/c])
         extenorc?]{
Convenience function for making an extenorc with no fields and a single struct-type-property.

@emph{Stability - less stable than the rest of this library!}
}

@subsection{APIs that should probably exist}

@itemlist[
@item{@tt{merge-extenors} -- this should have various options related to conflicts.  IE what equality predicate (if any) to decide whether a field/property is the same in both, when both extenors have field values that aren't equal should the merge raise an exception or should it prefer the field in one of the extenors, etc}
@item{@tt{remove-extenorc-with-field-name} -- remove the extenorc that has the given field name}
@item{@tt{remove-extenorc-with-property} -- remove the extenorc with the given struct-type-property}
@item{what else?}
]


@subsection{Library Extenorcs}

TODO - I want a library of useful extenorcs.  Probably mainly extenorcs that implement struct-type properties.

I have a couple written already, such as an extenorc that provides an implementation of @racket[prop:dict] and an one providing an implementation of @racket[prop:custom-write].
But they are not provided by any public modules yet.


@section[#:tag "stability"]{Stability}
Or, lack thereof.

I'm not yet willing to commit to anything here.
You can email me if you think I should change my mind about this.

Besides some things marked throughout this document as things I may potentially change, I may even change the names “extenor” and “extenorc”.  In particular, the two names are really similar and easy to confuse.

@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-extenor"]{on github}.

This library is distributed under the MIT license and the Apache version 2.0 license, at your option.
(IE same license as Racket.)
