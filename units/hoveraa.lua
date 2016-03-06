unitDef = {
  unitname            = [[hoveraa]],
  name                = [[Flail]],
  description         = [[Anti-Air Hovercraft]],
  acceleration        = 0.048,
  brakeRate           = 0.043,
  buildCostEnergy     = 300,
  buildCostMetal      = 300,
  builder             = false,
  buildPic            = [[hoveraa.png]],
  buildTime           = 300,
  canAttack           = true,
  canGuard            = true,
  canHover            = true,
  canMove             = true,
  canPatrol           = true,
  category            = [[HOVER]],
  collisionVolumeOffsets = [[0 0 -5]],
  collisionVolumeScales  = [[40 40 40]],
  collisionVolumeTest    = 1,
  collisionVolumeType    = [[ellipsoid]], 
  corpse              = [[DEAD]],

  customParams        = {
    description_de = [[Fulgabwehrgleiter]],
    description_pl = [[Poduszkowiec przeciwlotniczy]],
    helptext       = [[The Flail launches a single large, short-medium range SAM that does heavy damage.]],
	helptext_de    = [[Der Flail verschießt eine einzige, große Flugabwehrrakete auf mittlerer Distanz, die wirklich großen Schaden anrichtet.]],
	helptext_pl    = [[Flail wystrzeliwuje rakiety przeciwlotnicze sredniego zasiegu, ktore zadaja wysokie obrazenia.]],
	modelradius    = [[20]],
	midposoffset   = [[0 8 0]],
  },

  explodeAs           = [[BIG_UNITEX]],
  footprintX          = 3,
  footprintZ          = 3,
  iconType            = [[hoveraa]],
  idleAutoHeal        = 5,
  idleTime            = 1800,
  mass                = 200,
  maxDamage           = 1300,
  maxSlope            = 36,
  maxVelocity         = 3.54,
  minCloakDistance    = 75,
  movementClass       = [[HOVER3]],
  noAutoFire          = false,
  noChaseCategory     = [[TERRAFORM LAND SINK TURRET SHIP SATELLITE SWIM FLOAT SUB HOVER]],
  objectName          = [[hoveraa.s3o]],
  script              = [[hoveraa.lua]],
  seismicSignature    = 4,
  selfDestructAs      = [[BIG_UNITEX]],

  sfxtypes            = {

    explosiongenerators = {
      [[custom:HEAVYHOVERS_ON_GROUND]],
      [[custom:RAIDMUZZLE]],
    },

  },

  side                = [[CORE]],
  sightDistance       = 660,
  smoothAnim          = true,
  turninplace         = 0,
  turnRate            = 616,
  workerTime          = 0,

  weapons             = {

    {
      def                = [[WEAPON]],
      onlyTargetCategory = [[FIXEDWING GUNSHIP]],
    },

  },


  weaponDefs          = {

    WEAPON = {
      name                    = [[Medium SAM]],
      areaOfEffect            = 64,
      canattackground         = false,
      cegTag                  = [[missiletrailbluebig]],
      collideFriendly         = false,
      craterBoost             = 1,
      craterMult              = 2,
      cylinderTargeting       = 1,

	  customParams        	  = {
		isaa = [[1]],
		light_color = [[0.5 0.6 0.6]],
	  },

      damage                  = {
        default = 37.5,
        planes  = 375,
        subs    = 20.625,
      },

      edgeEffectiveness       = 0.5,
      explosionGenerator      = [[custom:STARFIRE]],
      fireStarter             = 100,
      fixedlauncher           = true,
      flightTime              = 6,
      impulseBoost            = 0,
      impulseFactor           = 0.4,
      interceptedByShieldType = 2,
      model                   = [[hovermissile.s3o]],
      noSelfDamage            = true,
      range                   = 800,
      reloadtime              = 5,
      smokeTrail              = true,
      soundHit                = [[weapon/missile/vlaunch_hit]],
      soundStart              = [[weapon/missile/missile_fire8]],
      startvelocity           = 200,
      texture2                = [[AAsmoketrail]],
      tolerance               = 4000,
      tracks                  = true,
      turnRate                = 64000,
      weaponAcceleration      = 300,
      weaponTimer             = 1,
      weaponType              = [[StarburstLauncher]],
      weaponVelocity          = 1400,
    },

  },


  featureDefs         = {

    DEAD  = {
      description      = [[Wreckage - Flail]],
      blocking         = false,
      category         = [[corpses]],
      damage           = 1300,
      energy           = 0,
      featureDead      = [[HEAP]],
      featurereclamate = [[SMUDGE01]],
      footprintX       = 3,
      footprintZ       = 3,
      height           = [[20]],
      hitdensity       = [[100]],
      metal            = 200,
      object           = [[hoveraa_dead.s3o]],
      reclaimable      = true,
      reclaimTime      = 200,
      seqnamereclamate = [[TREE1RECLAMATE]],
      world            = [[All Worlds]],
    },

    HEAP  = {
      description      = [[Debris - Flail]],
      blocking         = false,
      category         = [[heaps]],
      damage           = 1600,
      energy           = 0,
      featurereclamate = [[SMUDGE01]],
      footprintX       = 3,
      footprintZ       = 3,
      hitdensity       = [[100]],
      metal            = 100,
      object           = [[debris3x3c.s3o]],
      reclaimable      = true,
      reclaimTime      = 100,
      seqnamereclamate = [[TREE1RECLAMATE]],
      world            = [[All Worlds]],
    },

  },

}

return lowerkeys({ hoveraa = unitDef })
