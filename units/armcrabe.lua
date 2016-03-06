unitDef = {
  unitname               = [[armcrabe]],
  name                   = [[Crabe]],
  description            = [[Heavy Riot/Skirmish Spider - Curls into Armored Form When Stationary]],
  acceleration           = 0.22,
  brakeRate              = 0.18,
  buildCostEnergy        = 1600,
  buildCostMetal         = 1600,
  buildPic               = [[armcrabe.png]],
  buildTime              = 1600,
  canAttack              = true,
  canGuard               = true,
  canMove                = true,
  canPatrol              = true,
  canstop                = [[1]],
  category               = [[LAND]],
  collisionVolumeOffsets = [[0 0 0]],
  collisionVolumeScales  = [[60 60 60]],
  collisionVolumeType    = [[ellipsoid]],
  corpse                 = [[DEAD]],

  customParams           = {
    description_bp = [[Rob� dispersador pesado.]],
    description_es = [[Unidad pesante de alborote/escaramuzador]],
    description_fi = [[Raskas mellakka/kahakoitsijarobotti]],
    description_fr = [[Marcheur arachnide lourd]],
    description_it = [[Unita pesante da rissa/scaramucciatore]],
	description_de = [[Schwere Riot/Skirmish Spinne - Zieht sich bei Stillstand in seine Panzerung zur�ck]],
	description_pl = [[Ciezki pajak]],
	helptext       = [[The Crabe's huge shells obliterate large swarms of cheap units, and can also outrange basic defenses. When it stops walking, Crabe curls up into armored form reducing incoming damage to a third. The Crabe's main weakness is its lack of mobility.]],
    helptext_de    = [[Die gro�e Panzerung kann auch gro�e Gruppen von billigen Einheiten wegstecken, sowie grundlegende Verteidigung. Sobald die Spinne zum Stillstand kommt, zieht sie sich in die gepanzerte Form zur�ck: ein gewaltiger Verteidigungsturm. Der Nachteil ist die gro�e Bewegungseinschr�nkung.]],
    helptext_es    = [[Las balas enormes del Crabe arrasan pelotones de unidades baratas enemigas, y tienes alcance mayor que muchas defensas b�sicas. Cuando para de caminar, Crabe se enrosca en su forma acorazada. La debilidad principal del Crabe es su falta de movilidad.]],
    helptext_fi    = [[Craben massiiviset plasma-ammukset vahingoittavat yksik?it? laajalla alueella. Pys?htyess??n Crabe linnoittautuu v?hent?en itseens? kohdistuvaa vahinkoa.]],
    helptext_fr    = [[Le canon � plasma lourd du Crabe peut �liminer facilement les d�fenses basiques ainsi que des hordes d'unit�s �nemies l�g�res gr�ce � son importante aire d'effet. Lorsqu'il s'arr�te de marcher, le Crabe se replie sur lui-m�me, ses pattes formant une carapace blind�e autour de lui r�duisant les dommages qu'il re�oit � un tier. Sa faiblesse tient en sa lenteur, tant de d�placement que de tir.]],
    helptext_it    = [[I proiettili enrmi del crabe obliterano sciami di unita economiche, e ha un raggio maggiore di molte difese basiche. Quando smette di camminare, Crabe si raggomitola nella sua forma corazzata. La deboleza principale del crabe � la mancanza di mobilit�.]],
    helptext_pl    = [[Ciezkie pociski Craba niszcza grupy lekkich jednostek i maja wiekszy zasieg niz podstawowe wiezyczki. Gdy sie nie porusza, Crab zwija sie, otrzymujac tylko cwierc obrazen. Jego glowna wada jest niska ruchliwosc.]],
	aimposoffset   = [[0 0 0]],
	midposoffset   = [[0 -10 0]],
	modelradius    = [[30]],
  },

  damageModifier         = 0.33,
  explodeAs              = [[BIG_UNIT]],
  footprintX             = 4,
  footprintZ             = 4,
  iconType               = [[spidersupport]],
  idleAutoHeal           = 5,
  idleTime               = 1800,
  leaveTracks            = true,
  maxDamage              = 4000,
  maxSlope               = 36,
  maxVelocity            = 1.35,
  maxWaterDepth          = 22,
  minCloakDistance       = 75,
  movementClass          = [[TKBOT4]],
  moveState              = 0,
  noChaseCategory        = [[FIXEDWING GUNSHIP]],
  objectName             = [[ARMCRABE]],
  pushResistant          = 0,
  script                 = [[armcrabe.lua]],
  seismicSignature       = 4,
  selfDestructAs         = [[BIG_UNIT]],

  sfxtypes               = {

    explosiongenerators = {
    --  [[custom:ARMCRABE_FLARE]],
	  [[custom:LARGE_MUZZLE_FLASH_FX]],
      [[custom:ARMCRABE_FLASH]],
      [[custom:ARMCRABE_WhiteLight]],
    },

  },

  sightDistance          = 660,
  trackOffset            = 0,
  trackStrength          = 8,
  trackStretch           = 1,
  trackType              = [[crossFoot]],
  trackWidth             = 50,
  turnRate               = 600,

  weapons                = {

    {
      def                = [[ARM_CRABE_GAUSS]],
      onlyTargetCategory = [[SWIM LAND SINK TURRET FLOAT SHIP HOVER]],
    },

  },

  weaponDefs             = {

    ARM_CRABE_GAUSS = {
      name                    = [[Heavy Plasma Cannon]],
      areaOfEffect            = 200,
      craterBoost             = 0,
      craterMult              = 0.5,

      customParams            = {
		light_color = [[1.5 1.13 0.6]],
		light_radius = 450,
      },

      damage                  = {
        default = 600.5,
        subs    = 30,
      },

      edgeEffectiveness       = 0.3,
      explosionGenerator      = [[custom:ARMCRABE_EXPLOSION]],
      impulseBoost            = 0,
      impulseFactor           = 0.32,
      interceptedByShieldType = 1,
      noSelfDamage            = true,
      range                   = 600,
      reloadtime              = 4,
      soundHit                = [[weapon/cannon/cannon_hit3]],
      soundStart              = [[weapon/cannon/heavy_cannon2]],
      -- size = 5, -- maybe find a good size that is bigger than default
      turret                  = true,
      weaponType              = [[Cannon]],
      weaponVelocity          = 290,
    },

  },

  featureDefs            = {

    DEAD  = {
      description      = [[Wreckage - Crabe]],
      blocking         = true,
      damage           = 4000,
      featureDead      = [[HEAP]],
      footprintX       = 5,
      footprintZ       = 4,
      metal            = 640,
      object           = [[crabe_dead.s3o]],
      reclaimable      = true,
      reclaimTime      = 640,
    },

    HEAP  = {
      description      = [[Debris - Crabe]],
      blocking         = false,
      damage           = 4000,
      footprintX       = 3,
      footprintZ       = 3,
      metal            = 320,
      object           = [[debris3x3c.s3o]],
      reclaimable      = true,
      reclaimTime      = 320,
    },

  },

}

return lowerkeys({ armcrabe = unitDef })
