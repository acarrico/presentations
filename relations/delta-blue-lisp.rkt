#lang typed/racket/base

(provide
 weakest-strength
 weak-strength
 medium-strength
 strong-strength
 required-strength
 ;; structure constructors
 create-variable
 create-constraint
 create-method
 ;; delta-blue entry points
 add-constraint
 remove-constraint
 extract-plan-from-variables
 extract-plan-from-constraints
 execute-plan
 )

(define-type Strength Natural)
(define weakest-strength (ann 4 Strength))
(define weak-strength (ann 3 Strength))
(define medium-strength (ann 2 Strength))
(define strong-strength (ann 1 Strength))
(define required-strength (ann 0 Strength))

(define weaker >)

(struct Constraint
  ((variables : (Listof Variable)) ; the variables constrained by this constraint
   (strength : Strength)
   (input-flag : Boolean) ; true for input constraints
   (methods : (Listof Method)) ; the set of alternative methods for enforcing this constraint
   (selected-method : (Option Method)) ; the method used to enforce this constraint, if any
   )
  #:transparent
  #:mutable)

(define (create-constraint (variables : (Listof Variable))
                           (strength : Strength)
                           (input-flag : Boolean)
                           (methods : (Listof Method)))
  (Constraint variables strength input-flag methods #f))

;; A method of enforcing a constraint:
(struct Method
  ((code : (-> Void)) ; procedure that calculates the value of an output
   (index : Natural) ; index in the constraint's variable list of the output
   )
  #:transparent)

(define (create-method (code : (-> Void)) (output-index : Natural))
  (Method code output-index))

;; Variables are the glue of constraint graphs.
(struct Variable
  (#;(value : Value) ;; the value of this variable
   (constraints : (Listof Constraint)) ;; all constraints that reference this variable
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
   (mark : Mark)
   ;; For constant propagation:
   (stay : Boolean)) ; True if this variable is precomputed at planning time
  #:mutable
  #:transparent)

(define (create-variable)
  (Variable '() #f weakest-strength 0 #t))

(define (method->output-var (c : Constraint) (m : Method))
  (list-ref (Constraint-variables c) (Method-index m)))

(define (selected-method->output-var (c : Constraint))
  (define m (Constraint-selected-method c))
  (if m
      (method->output-var c m)
      (error "delta blue: expected a selected method in Constraint" c)))

(define #:forall (T)
  (fold-consuming-constraints
   (proc : (-> Constraint T T))
   (init : T)
   (v : Variable))
  : T
  (define var-determined-by-var (Variable-determined-by v))
  (for/fold : T
            ((acc : T init))
            ((c : Constraint (Variable-constraints v))
             #:when (and (not (eq? c var-determined-by-var))
                         (enforced? c)))
    (proc c acc)))

(define (inputs-always (c : Constraint) (predicate : (-> Variable Boolean)))
  (define output-var (selected-method->output-var c))
  (for/and : Boolean
           ((v (Constraint-variables c))
            #:when (not (eq? v output-var)))
    (predicate v)))

(define (enforced? (c : Constraint)) : Boolean
  (and (Constraint-selected-method c) #t))

;; Delta-Blue Entry Points

(define (add-constraint (c : Constraint)) : Void
  (set-Constraint-selected-method! c #f)
  (for ((v (Constraint-variables c)))
    (set-Variable-constraints! v (cons c (Variable-constraints v))))
  (incremental-add c))

(define (incremental-add (c : Constraint)) : Void
  (define mark (new-mark))
  (let loop : Void ((retracted : (Option Constraint) (enforce c mark)))
    (when retracted
      (loop (enforce retracted mark)))))

(define (select-method (c : Constraint) (mark : Mark))
  (define-values (best-out-strength best-out-method)
    (for/fold ((best-out-strength : Strength (Constraint-strength c))
               (best-out-method : (Option Method) #f))
              ((m (Constraint-methods c)))
      (let* ((out-var (method->output-var c m))
             (out-mark (Variable-mark out-var))
             (out-strength (Variable-walk-strength out-var)))
        (if (and (not (= mark out-mark))
                 (weaker out-strength best-out-strength))
          (values out-strength m)
          (values best-out-strength best-out-method)))))
  (set-Constraint-selected-method! c best-out-method)
  best-out-method)

(define (enforce (c : Constraint) (mark : Mark)) : (Option Constraint)
  (define selected-method (select-method c mark))
  (cond (selected-method
         (let* ((output-var (method->output-var c selected-method))
                (retracted (Variable-determined-by output-var)))
           (for ((input (Constraint-variables c))
                 #:when (not (eq? input output-var)))
             (set-Variable-mark! input mark))
           (when retracted
             (set-Constraint-selected-method! retracted #f))
           (set-Variable-determined-by! output-var c)
           (cond ((add-propagate c mark)
                  (set-Variable-mark!
                   ;; don't use output-var in case call to
                   ;; add-propagate may have changed c's selected
                   ;; method
                   (selected-method->output-var c) mark)
                  retracted)
                 (else
                  (error "delta blue: Cycle encountered" c)))))
        ((eq? required-strength (Constraint-strength c))
         (error "delta blue: Failed to enforce a required constraint"))
        (else
         #f)))

(define (add-propagate (c : Constraint) (mark : Mark))
  (let loop : Boolean ((todo : (Listof Constraint) (list c)))
    (cond ((pair? todo)
           (let* ((d (car todo))
                  (todo (cdr todo))
                  (selected-method (Constraint-selected-method d)))
             ;; ISSUE: the pseudo code doesn't do this check, and it can
             ;; probably be avoided:
             (if (not selected-method)
                 (error "delta blue: no selected method, this shouldn't happen!")
                 (let ((d-output-var (method->output-var d selected-method)))
                   (if (= mark (Variable-mark d-output-var))
                       (begin
                         (incremental-remove c)
                         #f)
                       (begin
                         (recalculate d d-output-var)
                         (loop ((inst fold-consuming-constraints (Listof Constraint))
                                cons todo d-output-var))))))))
          (else
           #t))))

(define (remove-constraint (c : Constraint))
  (if (enforced? c)
      (incremental-remove c)
      (for ((v (Constraint-variables c)))
        (set-Variable-constraints! v (remq c (Variable-constraints v))))))

(define (incremental-remove (c : Constraint)) : Void
  (let ((out (selected-method->output-var c)))
    (set-Constraint-selected-method! c #f)
    (for ((v (Constraint-variables c)))
      (set-Variable-constraints! v (remq c (Variable-constraints v))))
    (let ((unenforced (sort (remove-propagate-from out)
                            (lambda ((c1 : Constraint) (c2 : Constraint))
                              (weaker (Constraint-strength c2)
                                      (Constraint-strength c1))))))
      (for ((d unenforced))
        (incremental-add d)))))

(define (remove-propagate-from (out : Variable))
  (set-Variable-determined-by! out #f)
  (set-Variable-walk-strength! out weakest-strength)
  (set-Variable-stay! out #t)
  (let loop : (Listof Constraint)
       ((todo : (Listof Variable) (list out))
        (unenforced : (Listof Constraint) '()))
       (if (pair? todo)
           (let* ((v (car todo))
                  (unenforced (for/fold ((unenforced : (Listof Constraint) unenforced))
                                        ((c : Constraint (Variable-constraints v)))
                                (if (enforced? c)
                                    unenforced
                                    (cons c unenforced))))
                  (todo (fold-consuming-constraints
                         (lambda ((c : Constraint) (todo : (Listof Variable)))
                           (let ((output-var (selected-method->output-var c)))
                             (recalculate c output-var)
                             (cons output-var todo)))
                         (cdr todo)
                         v)))
             (loop todo unenforced))
           unenforced)))

;; plan extraction

(define (extract-plan-from-variables (variables : (Listof Variable)))
  (make-plan
   (for*/fold ((sources : (Listof Constraint) '()))
              ((v variables)
               (c (Variable-constraints v)))
     (if (and (Constraint-input-flag c)
              (enforced? c))
         (cons c sources)
         sources))))

(define (extract-plan-from-constraints (constraints : (Listof Constraint)))
  (make-plan
   (for/fold ((sources : (Listof Constraint) '()))
             ((c constraints))
     (if (and (Constraint-input-flag c)
              (enforced? c))
         (cons c sources)
         sources))))

(define (make-plan (sources : (Listof Constraint))) : (Listof Constraint)
  (define mark (new-mark))
  (let loop : (Listof Constraint) ((reverse-plan : (Listof Constraint) '())
                                   (hot : (Listof Constraint) sources))
       (if (pair? hot)
           (let* ((c (car hot))
                  (hot (cdr hot))
                  (v (selected-method->output-var c)))
             (if (and (not (= mark (Variable-mark v)))
                      (inputs-always c (lambda ((i : Variable))
                                         (or (Variable-stay i)
                                             (= mark (Variable-mark i))))))
                 (begin
                   (set-Variable-mark! v mark)
                   (loop (cons c reverse-plan)
                         ((inst fold-consuming-constraints (Listof Constraint)) cons hot v)))
                 (loop reverse-plan hot)))
           (reverse reverse-plan))))

;; utilities

;; recalculate takes the constraint to recalculate
;; AND the output variable of that constraint
;; (to avoid some calls to selected-method-output-var)
(define (recalculate (c : Constraint) (v : Variable)) : Void
  (set-Variable-walk-strength! v (output-walk-strength c))
  (set-Variable-stay! v (constant-output c))
  (when (Variable-stay v)
    (execute-selected-method c)))

(define (output-walk-strength (c : Constraint))
  (define selected-method-output-var (selected-method->output-var c))
  (for/fold ((min-strength : Strength (Constraint-strength c)))
            ((m : Method (Constraint-methods c)))
    : Strength
    (let ((output-var (method->output-var c m)))
      (if (eq? output-var selected-method-output-var)
          min-strength
          (let ((output-walk-strength (Variable-walk-strength output-var)))
            (if (weaker output-walk-strength min-strength)
                output-walk-strength
                min-strength))))))

(define (constant-output (c : Constraint))
  (and (not (Constraint-input-flag c))
       (inputs-always c Variable-stay)))

(define-type Mark Natural)

(define mark-counter : Mark (ann 0 Mark))

(define (new-mark) : Mark
  (set! mark-counter (+ mark-counter (ann 1 Mark)))
  mark-counter)

;; other functions not explicitly defined in the pseudo-code

(define (execute-selected-method (c : Constraint))
  (define m (Constraint-selected-method c))
  (if m
      ((Method-code m))
      (error "delta blue: execute-selected-method with no selected method" c)))

(define-type Plan (Listof Constraint))

(define (execute-plan (plan : Plan))
  ;; a plan is just a list of constraints, to be executed in order
  (for ((c plan))
    (execute-selected-method c)))
