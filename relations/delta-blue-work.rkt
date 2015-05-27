#lang typed/racket

;; level in the constraint hierarchy:
(define-type Strength Natural)
(define weakest (ann 3 Strength))
(define weak (ann 2 Strength))
(define strong (ann 1 Strength))
(define required (ann 0 Strength))

;; A Constraint on a set of variables:
(define-syntax (define-constraint stx)
  (syntax-case stx ()
    ((_ name ((method : type) ...) strength)
     (define-constraint name ((method : type) ...) strength #f))
    ((_ name ((method : type) ...) strength input?)
     (define name (Vector (-> type ... type) ...))))))

(struct Constraint
  ((variables : (Vectorof Variable)) ; the variables constrained by this constraint
   (strength : Strength)
   (input-flag : Boolean) ; true for input constraints
   (methods : Methods) ; the set of alternative methods for enforcing this constraint
   (selected-method : (Option Method)) ; the method used to enforce this constraint, if any
   )
  #:transparent
  #:mutable)

(define-type Constraints (Listof Constraint))

(define (empty-constraints) : Constraints '())



;; A method of enforcing a constraint:
(struct (Value) Method
  ((code : (-> Value)) ; procedure that calculates the value of an output
   (index : Natural) ; index in the constraint's variable list of the output
   ))

(define-type Methods (Listof Method))

;; Variables are the glue of constraint graphs.
(struct (Value) Variable
  ((value : Value) ;; the value of this variable
   (constraints : Constraints) ;; all constraints that reference this variable
   (determined-by : (Option Constraint)) ;; the constraint that determines this variable, if any.
   ;; For method selection and plan extraction:
   (walk-strength : Strength)
   ;; Mark uses:
   ;;
   ;; 1. During the process of adding a constraint to the hierarchy,
   ;; when a method is selected, its output variable is marked. This
   ;; prevents possible loops in which a pair of constraints are
   ;; alternately added and retracted.
   ;;
   ;; 2. Detect cycles in the constraint solution graph, in which case
   ;; DeltaBlue will remove the offending constraint.
   ;;
   ;; 3. During plan extraction to indicate which variables have been
   ;; computed by methods already in the plan, allowing DeltaBlue to
   ;; ensure that the inputs of a method will be computed before that
   ;; method is executed.
   (mark : Natural)
   ;; For constant propagation:
   (stay : Boolean)) ; True if this variable is precomputed at planning time
  #:mutable
  #:transparent)

(define #:forall (Value) (fresh-variable (v : Value)) : (Variable Value)
  ;; Variables are created as if they were determined by a virtual
  ;; stay constraint with a strength of weakest.
  (Variable v (empty-constraints) #f weakest 0 #t))

(define (add-constraint! (c : Constraint))
  (set-Constraint-selected-method! c #f)
  (for ((v (Constraint-variables c)))
    (set-Variable-constraints! v (cons c (Variable-constraints v))))
  (incremental-add c))

(define (incremental-add! (c : Constraint))
  (define mark (fresh-mark))
  (let loop ((retracted (enforce! c mark)))
    ...))

(define (enforce! (c : Constraint) (m : Mark))
  (select-method! c m)
  ...)

(define (select-method! (c : Constraint) (m : Mark))
  (set-Constraint-selected-method! c #f)
  

(define (remove-constraint hierarchy c)
  hierarchy)

(define (extract-plan-from-variables)
  (void))

(define (extract-plan-from-constraints)
  (void))

(define (execute-plan)
  (void))

(fresh-variable (ann 1 Natural))

