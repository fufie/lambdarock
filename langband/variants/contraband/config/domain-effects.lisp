(define-domain-effect '(mapping aerial)
    '(agnosca aeris))

(define-domain-effect '(identify liquid)
    '(agnosca aqua))

(define-domain-effect '(mapping plants)
    '(agnosca arbor))

(define-domain-effect '(probe plant)
    '(agnosca arbor))

(define-domain-effect '(probe beast)
    '(agnosca bestia))

(define-domain-effect '(probe fire-beast)
    '(agnosca ignis))

(define-domain-effect '(probe fire-item)
    '(agnosca ignis))

(define-domain-effect 'see-invisible
    '(agnosca imago))

(define-domain-effect '(detect lycantrophes)
    '(agnosca luna))

(define-domain-effect '(detect undead)
    '(agnosca mortis))

(define-domain-effect '(detect grave)
    '(or (agnosca mortis)
      (agnosca terra)))

(define-domain-effect '(mapping by-sun)
    '(agnosca sol))

(define-domain-effect '(detect gold)
    '(agnosca terra))

(define-domain-effect 'feather-fall
    '(amplio aeris))

(define-domain-effect '(amplify liquid)
    '(amplio aqua))

(define-domain-effect '(speed wooden-object)
    '(amplio arbor))

(define-domain-effect '(increase-deadliness wooden-object)
    '(amplio arbor))

(define-domain-effect '(speed flying-creature)
    '(or (amplio aeris)
      (amplio bestia)))

(define-domain-effect '(enlarge beast)
    '(amplio bestia))

(define-domain-effect '(increase-stat humanoid)
    '(amplio hominid))

(define-domain-effect '(enlarge humanoid)
    '(amplio hominid))

(define-domain-effect '(boost fire)
    '(amplio ignis))

(define-domain-effect '(enhance-senses humanoid)
    '(amplio imago))

(define-domain-effect '(enhance-senses night-creatures)
    '(or (amplio imago)
      (amplio luna)))

(define-domain-effect '(speed night-creatures)
    '(amplio luna))

(define-domain-effect '(age creature)
    '(amplio mortis))

(define-domain-effect '(aura sol creature)
    '(amplio sol))

(define-domain-effect '(transform iron steel)
    '(amplio terra))

(define-domain-effect '(summon air-elemental)
    '(arcesso aeris))

(define-domain-effect '(summon water-elemental)
    '(arcesso aqua))

(define-domain-effect '(summon plant wall)
    '(arcesso arbor))

(define-domain-effect '(summon steed)
    '(arcesso bestia))

(define-domain-effect '(summon guard)
    '(arcesso hominid))

(define-domain-effect '(summon terrain)
    '(arcesso imago))

(define-domain-effect '(summon shield reflecting)
    '(arcesso luna))

(define-domain-effect '(summon undead)
    '(arcesso mortis))

(define-domain-effect '(advance-time dawn)
    '(arcesso sol))

(define-domain-effect '(summon bridge)
    '(arcesso terra))

(define-domain-effect 'magical-stink
    '(creo aeris))

(define-domain-effect '(create drink)
    '(creo aqua))

(define-domain-effect '(create food)
    '(or (creo arbor)
      (creo bestia)))

(define-domain-effect '(create dove live)
    '(creo bestia))

(define-domain-effect 'magical-fire
    '(creo ignis))

(define-domain-effect '(scare creature)
    '(creo imago))

(define-domain-effect '(cause madness)
    '(creo luna))

(define-domain-effect '(create corpse animal)
    '(creo mortis))

(define-domain-effect '(light area)
    '(creo sol))

(define-domain-effect '(create sand)
    '(creo terra))

