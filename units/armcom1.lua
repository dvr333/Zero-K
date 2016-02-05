unitDef = {
  unitname               = [[armcom1]],
  name                   = [[Strike Commander]],
  description            = [[Mobile Assault Commander, Builds at 10 m/s]],
  acceleration           = 0.18,
  activateWhenBuilt      = true,
  amphibious             = [[1]],
  autoHeal               = 5,
  brakeRate              = 0.375,
  buildCostEnergy        = 1200,
  buildCostMetal         = 1200,
  buildDistance          = 120,
  builder                = true,

  buildoptions           = {
  },

  buildPic               = [[armcom.png]],
  buildTime              = 1200,
  canAttack              = true,
  canGuard               = true,
  canMove                = true,
  canPatrol              = true,
  canreclamate           = [[1]],
  canstop                = [[1]],
  category               = [[LAND]],
  collisionVolumeOffsets = [[0 0 0]],
  collisionVolumeScales  = [[45 50 45]],
  collisionVolumeTest    = 1,
  collisionVolumeType    = [[CylY]],
  commander              = true,
  corpse                 = [[DEAD]],

  customParams           = {
	cloakstealth = [[1]],
	description_de = [[Mobiler Sturmkommandant, Baut mit 10 M/s]],
	description_pl = [[Wszechstronny Dowodca, moc 10 m/s]],
	helptext       = [[The Strike Commander is a well-balanced command platform that can mount most modules, with decent speed, armor, and regeneration.]],
	helptext_de    = [[Der Strike Commander bietet ein sehr ausgeglichenes Gerüst, das mit den meisten Modulen ausgerüstet werden kann, mit anständiger Geschwindigkeit, Panzerung und Selbstreparierung.]],
	helptext_pl    = [[Strike to wszechstronny Dowodca, ktory moze uzywac wiekszosci modulow i ma dobra predkosc, wytrzymalosc i samonaprawe.]],
	level = [[1]],
	statsname = [[armcom1]],
	soundok = [[heavy_bot_move]],
	soundselect = [[bot_select]],
	soundbuild = [[builder_start]],
	commtype = [[1]],
	--decorationicons = {chest = "friendly", shoulders = "arrows-dot"},
    aimposoffset   = [[0 5 0]],
  },

  energyMake             = 6,
  energyStorage          = 0,
  energyUse              = 0,
  explodeAs              = [[ESTOR_BUILDINGEX]],
  footprintX             = 2,
  footprintZ             = 2,
  hideDamage             = false,
  iconType               = [[commander1]],
  idleAutoHeal           = 5,
  idleTime               = 1800,
  leaveTracks            = true,
  losEmitHeight       = 40,
  mass                   = 411,
  maxDamage              = 2500,
  maxSlope               = 36,
  maxVelocity            = 1.35,
  maxWaterDepth          = 5000,
  metalMake              = 4,
  metalStorage           = 0,
  minCloakDistance       = 75,
  movementClass          = [[AKBOT2]],
  noChaseCategory        = [[TERRAFORM SATELLITE FIXEDWING GUNSHIP HOVER SHIP SWIM SUB LAND FLOAT SINK TURRET]],
  norestrict             = [[1]],
  objectName             = [[ARMCOM]],
  script                 = [[armcom.lua]],
  seismicSignature       = 16,
  selfDestructAs         = [[ESTOR_BUILDINGEX]],

  sfxtypes               = {

    explosiongenerators = {
    	[[custom:BEAMWEAPON_MUZZLE_BLUE]],
		[[custom:NONE]],
    },

  },

  showNanoSpray          = false,
  showPlayerName         = true,
  side                   = [[ARM]],
  sightDistance          = 500,
  sonarDistance          = 300,
  trackOffset            = 0,
  trackStrength          = 8,
  trackStretch           = 1,
  trackType              = [[ComTrack]],
  trackWidth             = 26,
  terraformSpeed         = 600,
  turnRate               = 1148,
  upright                = true,
  workerTime             = 10,

  weapons                = {

    [1] = {
      def                = [[FAKELASER]],
      badTargetCategory  = [[FIXEDWING]],
      onlyTargetCategory = [[FIXEDWING LAND SINK TURRET SHIP SWIM FLOAT GUNSHIP HOVER]],
    },


    [5] = {
      def                = [[LASER]],
      badTargetCategory  = [[FIXEDWING]],
      onlyTargetCategory = [[FIXEDWING LAND SINK TURRET SHIP SWIM FLOAT GUNSHIP HOVER]],
    },

  },


  weaponDefs             = {

    FAKELASER     = {
      name                    = [[Fake Laser]],
      areaOfEffect            = 12,
      beamTime                = 0.1,
      coreThickness           = 0.5,
      craterBoost             = 0,
      craterMult              = 0,

      damage                  = {
        default = 0,
        subs    = 0,
      },

      duration                = 0.11,
      edgeEffectiveness       = 0.99,
      explosionGenerator      = [[custom:flash1green]],
      fireStarter             = 70,
      impactOnly              = true,
      impulseBoost            = 0,
      impulseFactor           = 0.4,
      interceptedByShieldType = 1,
      largeBeamLaser          = true,
      laserFlareSize          = 5.53,
      minIntensity            = 1,
      range                   = 300,
      reloadtime              = 0.11,
      rgbColor                = [[0 1 0]],
      soundStart              = [[weapon/laser/laser_burn5]],
      soundTrigger            = true,
      sweepfire               = false,
      texture1                = [[largelaser]],
      texture2                = [[flare]],
      texture3                = [[flare]],
      texture4                = [[smallflare]],
      thickness               = 5.53,
      tolerance               = 10000,
      turret                  = true,
      weaponType              = [[BeamLaser]],
      weaponVelocity          = 900,
    },


    LASER         = {
      name                    = [[Commander Laser]],
      areaOfEffect            = 12,
      beamTime                = 0.1,
      coreThickness           = 0.5,
      craterBoost             = 0,
      craterMult              = 0,

      damage                  = {
        default = 15.06,
        subs    = 0.8,
      },

      duration                = 0.11,
      edgeEffectiveness       = 0.99,
      explosionGenerator      = [[custom:flash1blue]],
      fireStarter             = 70,
      impactOnly              = true,
      impulseBoost            = 0,
      impulseFactor           = 0.4,
      interceptedByShieldType = 1,
      largeBeamLaser          = true,
      laserFlareSize          = 3,
      minIntensity            = 1,
      range                   = 300,
      reloadtime              = 0.11,
      rgbColor                = [[0 1 1]],
      soundStart              = [[weapon/laser/pulse_laser3]],
      soundTrigger            = true,
      texture1                = [[largelaser]],
      texture2                = [[flare]],
      texture3                = [[flare]],
      texture4                = [[smallflare]],
      thickness               = 3,
      tolerance               = 10000,
      turret                  = true,
      weaponType              = [[BeamLaser]],
      weaponVelocity          = 900,
    },

  },


  featureDefs            = {

    DEAD      = {
      description      = [[Wreckage - Strike Commander]],
      blocking         = true,
      damage           = 2500,
      energy           = 0,
      featureDead      = [[HEAP]],
      footprintX       = 3,
      footprintZ       = 3,
      metal            = 480,
      object           = [[armcom1_dead.s3o]],
      reclaimable      = true,
      reclaimTime      = 480,
    },

    HEAP      = {
      description      = [[Debris - Strike Commander]],
      blocking         = false,
      damage           = 2500,
      energy           = 0,
      footprintX       = 2,
      footprintZ       = 2,
      metal            = 240,
      object           = [[debris2x2c.s3o]],
      reclaimable      = true,
      reclaimTime      = 240,
    },


  },

}

return lowerkeys({ armcom1 = unitDef })
