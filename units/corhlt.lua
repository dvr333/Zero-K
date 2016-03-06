unitDef = {
  unitname                      = [[corhlt]],
  name                          = [[Stinger]],
  description                   = [[High-Energy Laser Tower]],
  acceleration                  = 0,
  brakeRate                     = 0,
  buildAngle                    = 4096,
  buildCostEnergy               = 420,
  buildCostMetal                = 420,
  builder                       = false,
  buildingGroundDecalDecaySpeed = 30,
  buildingGroundDecalSizeX      = 4,
  buildingGroundDecalSizeY      = 4,
  buildingGroundDecalType       = [[corhlt_aoplane.dds]],
  buildPic                      = [[CORHLT.png]],
  buildTime                     = 420,
  canAttack                     = true,
  canstop                       = [[1]],
  category                      = [[FLOAT TURRET]],
  collisionVolumeOffsets        = [[0 17 0]],
  collisionVolumeScales         = [[36 110 36]],
  collisionVolumeTest           = 1,
  collisionVolumeType           = [[CylY]],
  corpse                        = [[DEAD]],

  customParams                  = {
    description_fr = [[Tourelle Laser Moyenne HLT]],
	description_de = [[Hochenergetischer Laserturm]],
	description_pl = [[Ciezka wieza laserowa]],
    helptext       = [[The Stinger is a medium laser turret. Its three rotating laser guns can kill almost any small unit, but its low rate of fire makes it vulnerable to swarms when unassisted.]],
    helptext_fr    = [[Le Gaat Gun est compos? de trois canons lasers rotatifs lourd. Oblig?s de se refroidir apr?s chaque tir, il n'en d?livrent pas moins une forte puissance de feu instann?e. Tr?s utile sur des grosses cibles, elle aura besoin d'assistance en cas de nombreux ennemis.]],
	helptext_de    = [[Der Stinger ist ein durchschnittlicher Lasergesch?zturm. Seine drei rotierenden Laserkanonen k�nnen so gut wie jede kleine Einheit t�ten, aber die langsame Feuerrate macht den Stinger anf�llig f? gro�e Gruppen, sobald er nicht gen?end abgesichert ist.]],
	helptext_pl    = [[Stinger to wieza laserowa, ktora zadaje ciezkie obrazenia przy kazdym strzale i ma dosyc dobry zasieg, jednak dlugi czas przeladowania oznacza, ze latwo ja zniszczyc grupami mniejszych jednostek.]],
    aimposoffset   = [[0 55 0]],
  },

  explodeAs                     = [[MEDIUM_BUILDINGEX]],
  floater                       = true,
  footprintX                    = 3,
  footprintZ                    = 3,
  iconType                      = [[defenseheavy]],
  idleAutoHeal                  = 5,
  idleTime                      = 1800,
  levelGround                   = false,
  losEmitHeight                 = 80,
  mass                          = 267,
  maxDamage                     = 2475,
  maxSlope                      = 36,
  maxVelocity                   = 0,
  minCloakDistance              = 150,
  noAutoFire                    = false,
  noChaseCategory               = [[FIXEDWING LAND SHIP SATELLITE SWIM GUNSHIP SUB HOVER]],
  objectName                    = [[hlt.s3o]],
  script                        = [[corhlt.lua]],
  seismicSignature              = 4,
  selfDestructAs                = [[MEDIUM_BUILDINGEX]],

  sfxtypes                      = {

    explosiongenerators = {
      [[custom:HLTRADIATE0]],
      [[custom:beamlaser_hit_blue]],
    },

  },

  side                          = [[CORE]],
  sightDistance                 = 660,
  smoothAnim                    = true,
  turnRate                      = 0,
  useBuildingGroundDecal        = true,
  workerTime                    = 0,
  yardMap                       = [[ooo ooo ooo]],

  weapons                       = {

    {
      def                = [[LASER]],
      badTargetCategory  = [[FIXEDWING]],
      onlyTargetCategory = [[FIXEDWING LAND SINK TURRET SHIP SWIM FLOAT GUNSHIP HOVER]],
    },

  },


  weaponDefs                    = {

    LASER = {
      name                    = [[High-Energy Laserbeam]],
      areaOfEffect            = 14,
      beamTime                = 0.8,
      coreThickness           = 0.5,
      craterBoost             = 0,
      craterMult              = 0,
      
      customParams            = {
        statsprojectiles = 1,
        statsdamage = 850,
		
		light_color = [[0.25 0.25 0.75]],
		light_radius = 180,
      },

      damage                  = {
        default = 170.1,
        planes  = 170.1,
        subs    = 9,
      },

      explosionGenerator      = [[custom:flash1bluedark]],
      fireStarter             = 90,
	  fireTolerance           = 8192, -- 45 degrees
      impactOnly              = true,
      impulseBoost            = 0,
      impulseFactor           = 0.4,
      interceptedByShieldType = 1,
      largeBeamLaser          = true,
      laserFlareSize          = 10.4,
      minIntensity            = 1,
      noSelfDamage            = true,
      projectiles             = 5,
      range                   = 620,
      reloadtime              = 4.5,
      rgbColor                = [[0 0 1]],
      scrollSpeed             = 5,
      soundStart              = [[weapon/laser/heavy_laser3]],
      soundStartVolume        = 3,
      sweepfire               = false,
      texture1                = [[largelaserdark]],
      texture2                = [[flaredark]],
      texture3                = [[flaredark]],
      texture4                = [[smallflaredark]],
      thickness               = 10.4024486300101,
      tileLength              = 300,
      tolerance               = 10000,
      turret                  = true,
      weaponType              = [[BeamLaser]],
      weaponVelocity          = 2250,
    },

  },


  featureDefs                   = {

    DEAD  = {
      description      = [[Wreckage - Stinger]],
      blocking         = true,
      category         = [[corpses]],
      damage           = 2475,
      energy           = 0,
      featureDead      = [[HEAP]],
      featurereclamate = [[SMUDGE01]],
      footprintX       = 3,
      footprintZ       = 3,
      height           = [[20]],
      hitdensity       = [[100]],
      metal            = 168,
      object           = [[corhlt_d.s3o]],
      reclaimable      = true,
      reclaimTime      = 168,
      seqnamereclamate = [[TREE1RECLAMATE]],
      world            = [[All Worlds]],
    },
    

    HEAP  = {
      description      = [[Debris - Stinger]],
      blocking         = false,
      category         = [[heaps]],
      damage           = 2475,
      energy           = 0,
      featurereclamate = [[SMUDGE01]],
      footprintX       = 3,
      footprintZ       = 3,
      height           = [[4]],
      hitdensity       = [[100]],
      metal            = 84,
      object           = [[debris3x3a.s3o]],
      reclaimable      = true,
      reclaimTime      = 84,
      seqnamereclamate = [[TREE1RECLAMATE]],
      world            = [[All Worlds]],
    },

  },

}

return lowerkeys({ corhlt = unitDef })
