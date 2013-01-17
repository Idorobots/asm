###########################
# TinyCLOS port
############

###########################
# Some usefull bindings:
############

(macro i ()
       '(import 'samples.tinyclos))
(var defun function)
(var cond if*)
(var ??? 'unspecified-result)
(var car first)
(var cdr rest)
(var caar (lambda (x) (car (car x))))
(var cadr (lambda (x) (car (cdr x))))
(var cddr (lambda (x) (cdr (cdr x))))
(var cons join)
(var length length?)
(var defvar var)
(function make-vector (size)
          (vectorof (range 0 size)))

(function position-of-iter (what where n)
  (cond ((fnord? where) ())
        ((equal? what (car where)) n )
        ('else (position-of-iter what (cdr where) (+ n 1)))))

(function position-of (what where)
  (position-of-iter what where 0))

(function assq (what where)
  (cond ((fnord? where) ())
        ((is? what (caar where)) (car where))
        ('else (assq what (cdr where)))))

(function memq (what where)
  (cond ((fnord? where) ())
        ((is? what (car where)) where)
        ('else (memq what (cdr where)))))

(function last (list)
  (cond ((fnord? list) ())
        ((fnord? (cdr list)) (car list))
        ('else (last (cdr list)))))

(function every (f a b)
  (if (or (fnord? a) (fnord? b))
      'true
      (and (f (car a) (car b))
           (every f (cdr a) (cdr b)))))

(function every1 (f a)
  (if (fnord? a)
      'true
      (and (f (car a))
           (every1 f (cdr a)))))


############################
# Code starts here:
##########

(import 'imports.sort)

(defun __allocate-instance (class nfields)
  (do (defvar new (__allocate-instance-internal
                   class
                   ()
                   (lambda args
                     (error "Placeholder"))
                   nfields))
      # Boy, so many forward refs...
      (__set-instance-proc! new
                          (lambda (name)
                            (slot-ref new name)))
      (__lock-instance! new)
      new))

(defun __allocate-entity (class nfields)
  (__allocate-instance-internal
   class
   ()
   (lambda args
     (error "Tried to call an entity before its proc is set."))
   nfields))

(defvar __allocate-instance-internal  ???)
(defvar __instance?                   ???)
(defvar __instance-class              ???)
(defvar __set-instance-class-to-self! ???)
(defvar __set-instance-proc!          ???)
(defvar __instance-ref                ???)
(defvar __instance-set!               ???)
(defvar __lock-instance!              ???)

(scoped
 (do (defvar instances '())                           # ((closure . vector) ...)
   (defun get-vector (closure)
     (cadr (assq closure instances)))

   (defun locked? (closure)
     (nth (get-vector closure) 1))

   (set! __allocate-instance-internal
         (lambda (class lock proc nfields)
           (do (defvar vec (make-vector (+ nfields 3)))
               (defvar closure (lambda args
                                 (apply (nth vec 0) args)))
             (set! (nth vec 0) proc)
             (set! (nth vec 1) lock)
             (set! (nth vec 2) class)
             (set! instances (cons (tuple closure vec) instances))
            closure)))

   (set! __instance?
         (lambda (x) (not (fnord? (get-vector x)))))

   (set! __instance-class
         (lambda (closure)
           (nth (get-vector closure) 2)))

   (set! __set-instance-class-to-self!
         (lambda (closure)
           (set! (nth (get-vector closure) 2) closure)))

   (set! __set-instance-proc!
         (lambda (closure proc)
           (if (locked? closure)
               (error "Can't set procedure of the instance.")
               (set! (nth (get-vector closure) 0) proc))))

   (set! __instance-ref
         (lambda (closure index)
           (nth (get-vector closure) (+ index 3))))

   (set! __instance-set!
         (lambda (closure index new-value)
           (set! (__instance-ref closure index) new-value)))

     (set! __lock-instance!
           (lambda (closure)
             (set! (nth (get-vector closure) 1) 'true)))))

(defun class-of (x)
  (cond ((__instance? x) (__instance-class x))
        ((tuple? x)     Tuple)
        ((fnord? x)     Unit)
        ((symbol? x)    Symbol)
        ((function? x)  Function)
        ((number? x)    Number)
        ((vector? x)    Vector)
        ((set? x)       Set)
        ((string? x)    String)
        ('else          ())))

# initargs = ((slot value))
(defun make (class initargs)
  (cond ((or (is? class Class)
             (is? class EntityClass))
         (do (defvar new (__allocate-instance class (length the-slots-of-a-class)))
             (defvar dsupers (cadr (assoc 'direct-supers initargs)))
             (defvar dslots (map tuple
                                 (cadr (assoc 'direct-slots initargs))))
             (defvar cpl ((defun loop (sups so-far)
                            (if (fnord? sups)
                                (reverse so-far)
                                (loop (class-direct-supers (car sups))
                                      (cons (car sups) so-far))))
                          dsupers
                          (tuple new)))
             (defvar slots (apply append
                                  (cons dslots
                                        (map class-direct-slots (cdr cpl)))))
             (defvar nfields 0)
             (defvar field-initializers '())
             (defvar allocator (lambda (init)
                                 (do (defvar f nfields)
                                     (set! nfields (+ nfields 1))
                                     (set! field-initializers (cons init field-initializers))
                                     (tuple (lambda (o)   (__instance-ref o f))
                                            (lambda (o n) (__instance-set! o f n))))))
             (defvar getters-n-setters (map (lambda (s)
                                              (cons (car s)
                                                    (allocator (lambda () '()))))
                                            slots))
             (slot-set! new 'direct-supers dsupers)
             (slot-set! new 'direct-slots dslots)
             (slot-set! new 'cpl cpl)
             (slot-set! new 'slots slots)
             (slot-set! new 'nfields nfields)
             (slot-set! new 'field-initializers (reverse field-initializers))
             (slot-set! new 'getters-n-setters getters-n-setters)
             new))
         ((is? class Generic)
          (do (defvar new (__allocate-entity class (length (class-slots class))))
              (slot-set! new 'methods '())
              new))
         ((is? class Method)
          (do (defvar new (__allocate-instance class (length (class-slots class))))
              (slot-set! new 'specializers (cadr (assoc 'specializers initargs)))
              (slot-set! new 'procedure (cadr (assoc 'procedure initargs)))
              new))))

(defun slot-ref (object slot-name)
  ((nth (lookup-slot-info (class-of object) slot-name) 0) object))

(defun slot-set! (object slot-name new-value)
  ((nth (lookup-slot-info (class-of object) slot-name) 1) object new-value))

(defun lookup-slot-info (class slot-name)
  (do (defvar g-n-s (if (is? class Class)
                        getters-n-setters-for-class
                        (slot-ref class 'getters-n-setters)))
      (defvar entry (assoc slot-name g-n-s))
      (if entry
          (cdr entry)
          (error (append "No slot `" (stringof slot-name) "' in instances of `" (stringof class) "'")))))

(defun class-direct-slots (class)
  (slot-ref class 'direct-slots))

(defun class-direct-supers (class)
  (slot-ref class 'direct-supers))

(defun class-slots (class)
  (slot-ref class 'slots))

(defun class-cpl (class)
  (slot-ref class 'cpl))

(defun generic-methods (generic)
  (slot-ref generic 'methods))

(defun method-specializers (method)
  (slot-ref method 'specializers))

(defun method-procedure (method)
  (slot-ref method 'procedure))

(defvar the-slots-of-a-class
  '(direct-supers direct-slots cpl slots nfields field-initializers getters-n-setters))

(defvar getters-n-setters-for-class
  (do (defun make-em (s f)
        (tuple s
               (lambda (o)   (__instance-ref o f))
               (lambda (o n) (__instance-set! o f n))))
      (map (lambda (s)
             (make-em s (position-of s the-slots-of-a-class)))
           the-slots-of-a-class)))

(defvar Class (__allocate-instance () (length the-slots-of-a-class)))
(__set-instance-class-to-self! Class)

(defvar Top (make Class '((direct-supers ())
                          (direct-slots ()))))

(defvar Object (make Class `((direct-supers ($Top))
                             (direct-slots ()))))

(slot-set! Class 'direct-supers (tuple Object))
(slot-set! Class 'direct-slots (map tuple the-slots-of-a-class))
(slot-set! Class 'cpl (tuple Class Object Top))
(slot-set! Class 'slots (map tuple the-slots-of-a-class))
(slot-set! Class 'nfields (length the-slots-of-a-class))
(slot-set! Class 'field-initializers (map (lambda (s) (lambda () ()))
                                          the-slots-of-a-class))
(slot-set! Class 'getters-n-setters '())

(defvar FunctionClass (make Class `((direct-supers ($Class))
                               (direct-slots ()))))

(defvar EntityClass (make Class `((direct-supers ($FunctionClass))
                                  (direct-slots ()))))

(defvar Generic (make EntityClass `((direct-supers ($Object))
                                    (direct-slots (methods)))))

(defvar Method (make Class `((direct-supers ($Object))
                             (direct-slots (specializers procedure)))))

(defun make-class (direct-supers direct-slots)
  (make Class `((direct-supers $direct-supers)
                (direct-slots $direct-slots))))

(defun make-generic ()
  (make Generic ()))

(defun make-method (specializers procedure)
  (make Method `((specializers $specializers)
                 (procedure $procedure))))

(defvar initialize (make-generic))
(defvar allocate-instance (make-generic))
(defvar compute-getter-and-setter (make-generic))
(defvar compute-cpl (make-generic))
(defvar compute-slots (make-generic))
(defvar compute-apply-generic (make-generic))
(defvar compute-methods (make-generic))
(defvar compute-method-more-specific? (make-generic))
(defvar compute-apply-methods (make-generic))

(defvar generic-invocation-generics
  (tuple compute-apply-generic
         compute-methods
         compute-method-more-specific?
         compute-apply-methods))

(defun add-method (generic method)
  (do (slot-set! generic
                 'methods
                 (cons method
                       (select (slot-ref generic 'methods)
                               (lambda (m)
                                 (not (every is?
                                             (method-specializers m)
                                             (method-specializers method)))))))
      (__set-instance-proc! generic (compute-apply-generic generic))))

(__set-instance-proc! compute-apply-generic
                     (lambda (generic)
                       ((method-procedure (car (generic-methods generic))) () generic)))

(add-method compute-apply-generic
            (make-method (tuple Generic)
                         (lambda (call-next-method generic)
                           (lambda args
                             (if (and (memq generic generic-invocation-generics)
                                      (memq (car args) generic-invocation-generics))
                                 (apply (method-procedure (last (generic-methods generic)))
                                        (cons () args))
                                 ((compute-apply-methods generic)
                                  ((compute-methods generic) args)
                                  args))))))

(add-method compute-methods
            (make-method (tuple Generic)
                         (lambda (call-next-method generic)
                           (lambda (args)
                             (sort-list (lambda (m1 m2)
                                         ((compute-method-more-specific? generic)
                                        m1
                                        m2
                                        args))
                                   (select (generic-methods generic)
                                           (lambda (m)
                                             (every applicable?
                                                    (method-specializers m)
                                                    args))))))))

(add-method compute-method-more-specific?
            (make-method (tuple Generic)
                         (lambda (call-next-method generic)
                           (lambda (m1 m2 args)
                             ((defun loop (s1 s2 args)
                                (cond ((and (fnord? s1) (fnord? s2))
                                       (error "Two methods are qually specific."))
                                      ((or (fnord? s1) (fnord? s2))
                                       (error "Two methods have a different number of specializers."))
                                      ((fnord? args)
                                       (error "Fewer arguments than specializers."))
                                      ('else (do (defvar c1 (car s1))
                                                 (defvar c2 (car s2))
                                                 (defvar arg (car args))
                                                 (if (is? c1 c2)
                                                     (loop (cdr s1) (cdr s2) (cdr args))
                                                     (more-specific? c1 c2 arg))))))
                              (method-specializers m1)
                              (method-specializers m2)
                              args)))))

(add-method compute-apply-methods
            (make-method (tuple Generic)
                         (lambda (call-next-method generic)
                           (lambda (methods args)
                             (do (defun one-step (tail)
                                   (lambda ()
                                     (if (fnord? tail)
                                         (error "No applicable methods/next methods.")
                                         (apply (method-procedure (car tail))
                                                (cons (one-step (cdr tail))
                                                      args)))))
                                 ((one-step methods)))))))

(defun applicable? (c arg)
  (memq c (class-cpl (class-of arg))))

(defun more-specific? (c1 c2 arg)
  (memq c2 (memq c1 (class-cpl (class-of arg)))))

(add-method initialize
            (make-method (tuple Object)
                         (lambda (call-next-method object initargs)
                           object)))

(add-method initialize
            (make-method (tuple Class)
                         (lambda (call-next-method class initargs)
                           (do (call-next-method)
                               (slot-set! class
                                          'direct-supers
                                          (cadr (assoc 'direct-supers initargs)))
                               (slot-set! class
                                          'direct-slots
                                          (map (lambda (s)
                                                 (if (tuple? s) s
                                                     (tuple s)))
                                               (cadr (assoc 'direct-slots initargs))))
                               (slot-set! class
                                          'cpl
                                          (compute-cpl class))
                               (slot-set! class
                                          'slots
                                          (compute-slots class))
                               (defvar nfields 0)
                               (defvar field-initializers '())
                               (defun allocator (init)
                                 (do (defvar f nfields)
                                     (set! nfields (+ nfields 1))
                                     (set! field-initializers
                                           (cons init field-initializers))
                                     (tuple (lambda (o)   (__instance-ref o f))
                                            (lambda (o n) (__instance-set! o f n)))))
                               (defvar getters-n-setters
                                 (map (lambda (slot)
                                        (cons (car slot)
                                              (compute-getter-and-setter class
                                                                         slot
                                                                         allocator)))
                                      (slot-ref class 'slots)))
                               (slot-set! class 'nfields nfields)
                               (slot-set! class 'field-initializers field-initializers)
                               (slot-set! class 'getters-n-setters getters-n-setters)))))

(add-method initialize
            (make-method (tuple Generic)
                         (lambda (call-next-method generic initargs)
                           (do (call-next-method)
                               (slot-set! generic 'methods '())
                               (__set-instance-proc! generic
                                                    (lambda args (error "Has no methods.")))))))

(add-method initialize
            (make-method (tuple Method)
                         (lambda (call-next-method method initargs)
                           (do (call-next-method)
                               (slot-set! method
                                          'specializers
                                          (cadr (assoc 'specializers initargs)))
                               (slot-set! method
                                          'procedure
                                          (cadr (assoc 'procedure initargs)))))))

(add-method allocate-instance
            (make-method (tuple Class)
                         (lambda (call-next-method class)
                           (do (defvar field-initializers (slot-ref class 'field-initializers))
                               (defvar new (__allocate-instance class
                                                              (length field-initializers)))
                               (defun loop (n inits)
                                 (if (fnord? inits)
                                     new
                                     (do (__instance-set! new n ((car inits)))
                                         (loop (+ 1 n) (cdr inits)))))
                               (loop 0 field-initializers)))))

(add-method allocate-instance
            (make-method (tuple EntityClass)
                         (lambda (call-next-method class)
                           (do (defvar field-initializers (slot-ref class 'field-initializers))
                               (defvar new (__allocate-entity class
                                                             (length field-initializers)))
                               (defun loop (n inits)
                                 (if (fnord? inits)
                                     new
                                     (do (__instance-set! new n ((car inits)))
                                         (loop (+ 1 n) (cdr inits)))))
                               (loop 0 field-initializers)))))

(add-method compute-cpl
            (make-method (tuple Class)
                         (lambda (call-next-method class)
                           (compute-std-cpl class class-direct-supers))))

(defun compute-std-cpl (c get-direct-supers)
  (top-sort ((build-transitive-closure get-direct-supers) c)
            ((build-constraints get-direct-supers) c)
            (std-tie-breaker get-direct-supers)))

(defun top-sort (elements constraints tie-breaker)
  (do (defun loop (elements constraints result)
        (if (fnord? elements)
            result
            (do (defvar can-go-in-now
                        (select elements
                                (lambda (x)
                                  (every1 (lambda (constraint)
                                            (or (not (is? (cadr constraint) x))
                                                (memq (car constraint) result)))
                                          constraints))))
                (if (fnord? can-go-in-now)
                    (error "Invalid constraints")
                    (do (defvar choice (if (fnord? (cdr can-go-in-now))
                                           (car can-go-in-now)
                                           (tie-breaker result can-go-in-now)))
                        (loop (select elements
                                      (lambda (x) (not (is? x choice))))
                              constraints
                              (append result (tuple choice))))))))
      (loop elements constraints '())))

(defun std-tie-breaker (get-supers)
  (lambda (partial-cpl min-elts)
    (do (defun loop (pcpl)
          (do (defvar current-elt (car pcpl))
              (defvar ds-of-ce (get-supers current-elt))
              (defvar common (select min-elts
                                     (lambda (x)
                                       (memq x ds-of-ce))))
              (if (fnord? common)
                  (if (fnord (cdr pcpl))
                      (error "Nothing valid.")
                      (loop (cdr pcpl)))
                  (car common))))
        (loop (reverse partial-cpl)))))

(defun build-transitive-closure (get-follow-ons)
  (lambda (x)
    (do (defun track (result pending)
          (if (fnord? pending)
              result
              (do (defvar next (car pending))
                  (if (memq next result)
                      (track result (cdr pending))
                      (track (cons next result)
                             (append (get-follow-ons next)
                                     (cdr pending)))))))
        (track '() (tuple x)))))

(defun build-constraints (get-follow-ons)
  (lambda (x)
    (do (defun loop (elements this-one result)
          (if (or (fnord? this-one)
                  (fnord? (cdr this-one)))
              (if (fnord? elements)
                  result
                  (loop (cdr elements)
                        (cons (car elements)
                              (get-follow-ons (car elements)))
                        result))
              (loop elements
                    (cdr this-one)
                    (cons (tuple (car this-one) (cadr this-one))
                          result))))
        (loop ((build-transitive-closure get-follow-ons) x)
              '() '()))))

(add-method compute-slots
            (make-method (tuple Class)
                         (lambda (call-next-method class)
                           (do (defun collect (to-process result)
                                 (if (fnord? to-process)
                                     (reverse result)
                                     (do (defvar current (car to-process))
                                         (defvar name (car current))
                                         (defvar others '())
                                         (defvar remaining-to-process
                                           (select (cdr to-process)
                                                   (lambda (o)
                                                     (if (equal? (car o) name)
                                                         (do (set! others (cons o others))
                                                             ())
                                                         'true))))
                                         (collect remaining-to-process
                                                  (cons (append current
                                                                (apply append (map cdr others)))
                                                        result)))))
                               (collect (apply append (map class-direct-slots (class-cpl class)))
                                        ())))))

(add-method compute-getter-and-setter
            (make-method (tuple Class)
                         (lambda (call-next-method class slot allocator)
                           (allocator (lambda () ())))))

(set! make
      (lambda (class initargs)
        (do (defvar instance (allocate-instance class))
            (initialize instance initargs)
            instance)))

(defvar PrimitiveClass
  (make Class `((direct-supers ($Class))
                (direct-slots ()))))

(defun make-primitive-class class
  (make (if (fnord? class) PrimitiveClass (car class))
        `((direct-supers ($Top))
          (direct-slots ()))))

(defvar Tuple         (make-primitive-class))
(defvar Unit          (make-primitive-class))
(defvar Symbol        (make-primitive-class))
(defvar Function      (make-primitive-class FunctionClass))
(defvar Number        (make-primitive-class))
(defvar Vector        (make-primitive-class))
(defvar Set           (make-primitive-class))
(defvar String        (make-primitive-class))

##################
# Stuff for convinience:
#######

(macro defclass (name supers @tuple body)
  `(var $name (make Class (tuple (tuple 'direct-supers $(cons 'tuple supers))
                                 (tuple 'direct-slots $(cons 'tuple body))))))

(macro defgeneric (name)
  `(var $name (make-generic)))

(macro defmethod (name args @tuple body)
  `(add-method $name
     (make-method $(cons 'tuple (select (map (lambda (x)
                                               (if (tuple? x)
                                                   (cadr x)))
                                             args)
                                        (lambda (x) x)))
                  (lambda $(cons 'call-next-method (map (lambda (x)
                                                          (if (tuple? x)
                                                              (car x)
                                                              x))
                                                        args))
                          $(cons 'do body)))))

(defun pair (list acc)
  (if (fnord? list)
      (reverse acc)
      (pair (cddr list)
            (cons (tuple (car list)
                         (cadr list))
                  acc))))

(defun new args
  (make (car args)
        (pair (cdr args) '())))

(defun initialize-slots (object initargs)
  (map (lambda (slot)
         (do (defvar val (assoc (car slot) initargs))
             (when val
               (slot-set! object (car slot) (cadr val)))))
       (class-slots (class-of object))))

##################
# Tests
#######

(defclass Point (Object)
  'x 'y)

(defmethod initialize ((p Point) initargs)
  (call-next-method)
  (initialize-slots p initargs))

(defclass Point3D (Point)
  'z)

(defvar p1 (new Point 'x 1 'y 2))
(defvar p2 (new Point 'x 2 'y 3))
(defvar p3 (new Point3D 'x 5 'y 5 'z 5))

(defgeneric distance)
(defmethod distance ((a Point) (b Point))
  (write "Point * Point\n")
  (distance (new Point3D 'x (a 'x) 'y (a 'y) 'z 0)
            (new Point3D 'x (b 'x) 'y (b 'y) 'z 0)))

(defmethod distance ((a Point) (b Point3D))
  (write "Point * Point3D\n")
  (distance b a))

(defmethod distance ((a Point3D) (b Point))
  (write "Point3D * Point\n")
  (distance a (new Point3D 'x (b 'x) 'y (b 'y) 'z 0)))

(defmethod distance ((a Point3D) (b Point3D))
  (write "Point3D * Point3D\n")
  (defvar dx (- (b 'x) (a 'x)))
  (defvar dy (- (b 'y) (a 'y)))
  (defvar dz (- (b 'z) (a 'z)))
  (pow (reduce + (tuple (* dx dx) (* dy dy) (* dz dz)) 0) 0.5))

(distance p1 p2)
(distance p3 p1)
(distance p2 p3)
(distance p3 p3)