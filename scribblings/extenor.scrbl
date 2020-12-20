#lang scribble/manual
@title[#:tag "Extenor"]{Extenor}
@author+email["William Hatch" "william@hatch.uno"]

@(require
  scribble/examples
  racket/sandbox
  (for-label
   "../main.rkt"
   racket/base
   )
  )

@defmodule[extenor]

@section{Guide}

The Extenor package is an experimental package providing a particular kind of extensible object.
ExteNoR stands for “Extensible Nominal Record”, and an extenor can be an instance of multiple extenorcls, or “Extensible Nominal Record Class”.
An extenorcl is somewhat like a struct-type or a class, in that it defines a set of fields that an instance must have, as well as struct-type-properties.
Each extenorcl comes with a predicate that is true for extenors that are instances of that extenorcl, as well as setter and getter functions.
Each extenor is immutable, and using a setter or extending an extenor with a new extenorcl are functional opetations that return a fresh extenor.

The main motivation behind this library is to have something like structs that are extensible while still being functionally updatable.
Racket structs may be subtyped, so you can extend any given struct with new fields and struct-type-properties.
However, if you ever use a functional setter of the original struct-type on your extended struct instance, the object returned will be of the non-extended struct-type!
Extenors keep their extensions when they are functionally updated by functions that don't know about their extensions.

However, extenors are also an experiment with some other ideas in the hopes of making them particularly useful in conjunction with the Rash language.
In particular, extenorcl fields have “visibility”.
Hidden extenorcl fields are more like struct fields -- they can only be accessed with accessor functions specific to the extenorcl.
Visible extenorcl fields may also be accessed and updated via @racket[extenor-ref] and @racket[extenor-set].
Visible extenor fields are more duck-typable (or perhaps structurally-typable, my dear pedantic PL nerd friends).
The main motivation here is that you might have various shell utility functions that you use in pipelines that operate on standard field names, and you may add fields dynamically in a pipeline to make data from some system administration function/command work with some other command.
Thus extenors give you something that simultaneously has some of the properties of a dictionary and some of the properties of a nominal record, and in particular the capability to have struct-type-properties (and in fact different struct-type-properties on each instance).

@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base)))

@(examples
  #:eval my-evaluator
  (require extenor)
  (code:comment "Let's make some extenorcls, IE extenor classes.")
  (define-extenorcl point ([hidden x] [hidden y]))
  (define-extenorcl frog ([visible size] [visible name]))
  (define point-frog-v1 (extenor-extend (extenor-extend empty-extenor point 5 7)
                                           frog 10 "Jeremy"))
  (code:comment "Without a custom write procedure, extenors don't print very well.")
  (println point-frog-v1)
  (require extenor/extenorcl/custom-write-extenorcl)
  (define point-frog (extenor-extend point-frog-v1 prop:custom-write-extenorcl))
  (code:comment "This custom write implementation makes it print visible fields.")
  (println point-frog)
  (code:comment "point-frog is both a point and a frog.")
  (point? point-frog)
  (frog? point-frog)
  (code:comment "We can access the point information with struct-like accessors.")
  (point-x point-frog)
  (point-y point-frog)
  (code:comment "We can access frog info that way too.")
  (frog-size point-frog)
  (code:comment "But we can also access visible names with extenor-ref.")
  (extenor-ref point-frog 'size)
  (code:comment "Hidden fields can't be accessed with extenor-ref or extenor-set.")
  (eval:error (extenor-ref point-frog 'x))
  (code:comment "We can also functionally update.")
  (define bigger-frog (extenor-set point-frog 'size 100))
  (println bigger-frog)
  (define biggest-frog (set-frog-size point-frog 9000))
  (println biggest-frog)
  (code:comment "Interned symbols can be used as extenorcls.")
  (define blue-frog (extenor-extend point-frog 'color "blue"))
  (println blue-frog)
  (code:comment "extenor-set with a name that's not in the extenor extends it")
  (code:comment "like extenor-extend with a plain symbol.")
  (define green-frog (extenor-set point-frog 'color "green"))
  (println green-frog)
  (code:comment "Because the fields of point are hidden, they don't conflict")
  (code:comment "with other extenorcls that have (hidden or visible) fields named")
  (code:comment "x or y.")
  (code:comment "This example just causes confusion, but this can allow encapsulation")
  (code:comment "of private fields without needing to craft names that nobody else")
  (code:comment "will use.")
  (define extra-x (extenor-set point-frog 'x 'x-value))
  (println extra-x)
  (extenor-ref extra-x 'x)
  (point-x extra-x)
  (code:comment "Extenors are dict-like.  Let's make a frog implement prop:dict.")
  (require racket/dict)
  (require extenor/extenorcl/dict-extenorcl)
  (define dict-frog (extenor-extend green-frog prop:dict-extenorcl))
  (dict-ref dict-frog 'size)
  (code:comment "Actually, it's probably better to start with a more capable")
  (code:comment "extenor than the raw empty-extenor.")
  (define base-extenor
    (extenor-extend (extenor-extend empty-extenor prop:custom-write-extenorcl)
                    prop:dict-extenorcl))
  (code:comment "Maybe we will get related extenors from multiple sources and")
  (code:comment "want to join the data.")
  (define a-point (extenor-extend base-extenor point 42 24))
  (define a-frog (extenor-extend base-extenor frog 3 "Toad"))
  (define yet-another-point-frog
    (extenor-simple-merge a-point a-frog))
  (point? yet-another-point-frog)
  (frog? yet-another-point-frog)
  (println yet-another-point-frog)
  )


@section{Reference}

See @secref{stability}.


@defproc[(extenor? [v any/c]) bool?]{
Predicate - is this an extenor?
}

@defthing[empty-extenor extenor?]{
The empty extenor.
}

@defproc[(extenor-extend [an-extenor extenor?] [an-extenorcl extenorcl?]
                         [field-val any/c] ...)
         extenor?]{
Add an extenorcl to an extenor.
The @racket[field-vals] must appropriately match the fields of the extenorcl.
}

@defproc[(extenor-ref [an-extenor extenor?] [field-name symbol?]
                      [fallback any/c (λ () (error 'extenor-ref))])
         any/c]{
Get a visible field out of an extenor.
Note that the @racket[field-name] must be an interned symbol.
The @racket[fallback] optional argument acts like with @racket[hash-ref], if it is a procedure it is executed with no arguments, but as a convenience non-procedures may be provided and returned as-is.
}

@defproc[(extenor-set [an-extenor extenor?]
                      [field-name symbol?]
                      [new-value any/c])
         extenor?]{
Non-destructively (functionally) update a visible field of an extenor.
IE get a new extenor with the field value replaced.
If the extenor previously had no visible field, this effectively adds a degenerate extenorcl providing that visible field.
}

@defproc[(extenor-keys [an-extenor extenor?]) (listof symbol?)]{
Returns a list of keys for visible extenor fields.
}
@defproc[(extenor-struct-type-properties [an-extenor extenor?])
         (listof struct-type-property?)]{
Returns a list of struct-type-properties supported by the extenor.
}


@defproc[(extenor-remove-extenorcl [the-extenor extenor?] [the-extenorcl extenorcl?])
         extenor?]{
Returns an extenor like @racket[the-extenor] but with @racket[the-extenorcl] removed.
}
@defproc[(extenor-remove-extenorcl-with-key [the-extenor extenor?]
                                            [key (and/c symbol? symbol-interned?)])
         extenor?]{
Returns an extenor like @racket[the-extenor] but with the extenorcl that provides @racket[key] removed.
Note that this may remove multiple keys and/or struct-type-properties.
}
@defproc[(extenor-remove-extenorcl-with-struct-type-property
          [the-extenor extenor?]
          [stp struct-type-property?])
         extenor?]{
Returns an extenor like @racket[the-extenor] but with the extenorcl that provides @racket[stp] removed.
Note that this may remove multiple keys and/or struct-type-properties.
}

@defproc[(extenor-simple-merge [l extenor?] [r extenor?]
                               [#:equality equality
                                (or/c #t #f
                                      (-> any/c any/c any/c))
                                equal?])
         extenor?]{
Merges two extenors.
If @racket[l] and @racket[r] contain different extenorcls that provide the same visible key or structure-type-property, @racket[extenor-simple-merge] will raise an exception.
If @racket[l] and @racket[r] share any extenorcls, the values of the extenorcls will be compared with @racket[equality].
If @racket[equality] returns a non-@racket[#f] value (or if equality is @racket[#t]), the value in @racket[r] is used and the value of @racket[l] is discarded.
If @racket[equality] returns @racket[#f] (or if equality is @racket[#f]), an exception is raised.
}

@defproc[(extenorcl? [v any/c]) bool?]{
Predicate -- is this an extenorcl?
}
@defproc[(extenorcl-name [an-extenorcl extenorcl?]) any/c]{
Get the name of an extenorcl.

@emph{Stability - less stable than the rest of this library!}
}

@defproc[(extenorcl-struct-type-properties [the-extenorcl extenorcl?])
         (listof struct-type-property?)]{
Return a list of the struct-type-properties supported by @racket[the-extenorcl].
}

@defproc[(make-extenorcl [#:name name (or/c #f (and/c symbol? symbol-interned?)) #f]
                         [#:guard guard (or/c #f procedure?) #f]
                         [#:properties properties
                          (hash/c struct-type-property? any/c)
                          (hash)]
                         [field-name-spec
                          (listof (cons/c (or/c 'hidden 'visible)
                                          (and/c symbol? symbol-interned?)))]
                         ...)
         (list/c extenorcl?
                 (-> any/c any/c)
                 (listof (-> extenor? any/c))
                 (listof (-> extenor? any/c extenor?)))]{
Unless you want to dynamically generate extenorcls, you probably want @racket[define-extenorcl] instead.

Make an extenorcl.
This returns a list containing:
@itemlist[
@item{An extenorcl object.}
@item{A predicate for extenors containing the extenorcl object.}
@item{A list of getter functions that operate on extenors containing the extenorcl.}
@item{A list of setter functions that operate on extenors containing the extenorcl.  Note that they are functional setters that return a new extenor with an updated field, not a mutational setter.  Extenors are all immutable, though they may have mutable values in their fields.}
]


The @racket[name] argument provides a name for the extenorcl.
@emph{Stability - the name field is less stable than the rest of this library!}

The @racket[guard] can be a procedure that guards what values can be set in the extenorcl (both via the setter returned from this function and from @racket[extenor-set]) .
The @racket[guard] must be a procedure that accepts a value for each field of the extenorcl and must return a list of the same size.
Guards are run for @racket[extenor-extend], for the setter of any field from the extenorcl, or when @racket[extenor-set] is used on a field from the extenorcl.
In other words, it can arbitrarily interpose on field setting, though this is probably not advisable.
It can also raise an exception, which is probably the better thing to do when given input that is improper for the extenorcl.
@emph{Stability - I may change the API for guards.  As it is the guard can't tell which field is being set.  I want guards to be able to access all fields, so they can guard fields that have invariants relying on each other, but I also want a guard to have easy access to check an individual field.}

Each @racket[field-name-spec] is a pair specifying field visibility and field name.
Each field name must be an interned symbol.
Field visibility is either @racket['hidden] or @racket['visible].
Hidden fields can be accessed with a getter and setter returned by @racket[make-extenorcl], but not with @racket[extenor-ref] or @racket[extenor-set].
While each hidden field within a single extenorcl must have a unique name, different extenorcls can use the same hidden field name.
Visible fields can be accessed with @racket[extenor-ref] and @racket[extenor-set] (though setting a field may trigger a guard).
Visible fields in one extenorcl may conflict with another, so that an extenor can only have one of the two extenorcls.

TODO - extenorcls should also support generics.  However, at the time of writing generics have only a static interface.  Properties can be added to dynamically generated structs via @racket[make-struct], while generics can't be.  This is poor, and despite wording in many parts of the Racket docs, generics should not be preferred over struct-type-properties until they have a similar dynamic interface.
}

@defform[(define-extenorcl name (field-spec ...) keyword-arg ...)
         #:grammar ([field-spec (code:line field-name)
                                (code:line [hidden field-name])
                                (code:line [visible field-name])]
                    [keyword-arg (code:line #:guard guard-expression)
                                 (code:line #:property prop-expr val-expr)]
                    )]{
Static definition form wrapping @racket[make-extenorcl].
Note that the @racket[#:property] clause can be used multiple times to add multiple properties to the extenorcl.

Here's an example:
@racketblock[
             (define-extenorcl frog (weight [hidden poisonous?] [visible name])
               #:property prop:custom-write
               (λ (self port mode) (fprintf port "<Frog: name=~v>" (frog-name self)))
               #:property prop:procedure
               (λ (self . args)
                 (extenor-set self weight
                              (apply + (frog-weight self) args))))
             ]

This would define:
@itemlist[
@item{@tt{frog} as an extenorcl}
@item{@tt{frog?} as a predicate for extenors that contain the extenorcl}
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
               (extenor-extend empty-extenor frog 25 #f "arnold"))
             (define fat-arnold (arnold-the-frog 5 10 50))
             (code:comment "Returns 90")
             (extenor-ref fat-arnold 'weight)
             ]


}

@defproc[(make-prop-extenorcl [some-property struct-type-property?] [prop-val any/c])
         extenorcl?]{
Convenience function for making an extenorcl with no fields and a single struct-type-property.

@emph{Stability - less stable than the rest of this library!}
}

@subsection{APIs that should probably exist}

@itemlist[
@item{@tt{merge-extenors} -- this should have various options related to conflicts.  IE what equality predicate (if any) to decide whether a field/property is the same in both, when both extenors have field values that aren't equal should the merge raise an exception or should it prefer the field in one of the extenors, etc}
@item{what else?}
]


@subsection{Library Extenorcls}

@subsection{prop:custom-write}
@defmodule[extenor/extenorcl/custom-write-extenorcl]
@defthing[prop:custom-write-extenorcl extenorcl?]{
An extenorcl providing a prop:custom-write implementation that displays visible keys with their values.
Useful for manually inspecting an extenorcl since by default they are opaque.
}

@subsection{prop:dict}
@defmodule[extenor/extenorcl/dict-extenorcl]
@defthing[prop:dict-extenorcl extenorcl?]{
An extenorcl providing a prop:dict implementation.
It basically uses @racket[extenor-ref] and @racket[extenor-set], and as such only accepts interned symbols as keys.
}


@section[#:tag "stability"]{Stability}
Or, lack thereof.

I'm not yet willing to commit to anything here.
You can email me if you think I should change my mind about this.

Besides some things marked throughout this document as things I may potentially change, I may even change the names “extenor” and “extenorcl”.  In particular, the two names are really similar and easy to confuse.

@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-extenor"]{on github}.

This library is distributed under the MIT license and the Apache version 2.0 license, at your option.
(IE same license as Racket.)
