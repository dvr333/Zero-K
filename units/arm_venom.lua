unitDef = {
  unitname               = [[arm_venom]],
  name                   = [[Venom]],
  description            = [[Lightning Riot Spider]],
  acceleration           = 0.26,
  brakeRate              = 0.26,
  buildCostEnergy        = 200,
  buildCostMetal         = 200,
  buildPic               = [[arm_venom.png]],
  buildTime              = 200,
  canAttack              = true,
  canGuard               = true,
  canMove                = true,
  canPatrol              = true,
  canstop                = [[1]],
  category               = [[LAND]],
  collisionVolumeOffsets = [[0 0 0]],
  collisionVolumeScales  = [[38 38 38]],
  collisionVolumeTest    = 1,
  collisionVolumeType    = [[ellipsoid]], 
  corpse                 = [[DEAD]],

  customParams           = {
    description_bp = [[Aranha de PEM dispersadora]],
    description_es = [[Ara?a PEM de alborote]],
    description_fi = [[EMP-mellakkarobotti]],
    description_fr = [[Araign�e � effet de zone EMP]],
    description_it = [[Ragno PEM da rissa]],
    description_de = [[Unterst�tzende EMP Spinne]],
    description_pl = [[Pajak wsparcia EMP]],
    helptext       = [[The Venom is an all-terrain unit designed to paralyze enemies so other units can easily destroy them. It moves particularly fast for a riot unit and in addition to paralysis it does a small amount of damage. Works well in tandem with the Recluse to keep enemies from closing range with the fragile skirmisher.]],
    helptext_bp    = [[Venon � uma unidade escaladora projetada para paralizar inimigos para que outras unidades possam destru�los facilmente. Seus tiros podem atingir m�ltiplas unidades e portanto � �til como dispersadora. Funciona bem junto com o Recluse para impedir os inimigos de se aproximarem deste.]],
    helptext_es    = [[El Venom es una unidad all-terrain hecha para paralizar a los nemigos, permitiendo que otras unidades puedan destruirlos f�cilmente. Tiene AdE y es �til como unidad de alboroto, para tener a la larga pelotones de enemigos. Funciona bien juntado con los recluse para no dejar que los enemigos se acerquen demasiado al fr�gil escaramuzador.]],
    helptext_fi    = [[Maastokelpoinen Venom kykenee EMP-aseellaan halvaannuttamaan vihollisen yksik?t niin, ett? ne voidaan tuhota vaaratta. Tehokas toimiessaan yhdess? Recluse:n kanssa. Tuhoutuu nopeasti vihollisen tuliksen alla.]],
    helptext_fr    = [[Le Venom est une araign�e tout terrain rapide sp�cialement con�ue pour paralyser l'ennemi afin que d'autres unit�s puissent les d�truire rapidement et sans risques. Sa faible port�e est compens�e par son effet de zone pouvant affecter plusieurs unit�s � proximit� de sa cible. Est particuli�rement efficace en tandem avec le Recluse ou l'Hermit.]],
    helptext_it    = [[Il Venom � un'unita all-terrain fatta per paralizzare i nemici cosi che altre unita le possano distruggere facilmente. Ha un AdE ed � utile come unita da rissa, per tenere lontano sciame di nemici. Funziona bene con i recluse per non peremttere ai nemici di avvicinarsi troppo al fragili scaramuzzatore.]],
	helptext_de    = [[Venom ist eine gel�ndeunabh�ngige Einheit, welche Gegner paralysieren kann, damit andere Einheiten diese einfach zerst�ren k�nnen. Venom besitzt eine AoE und ist n�tzlich, um gengerische Schw�rme in Schach zu halten.]],
	helptext_pl    = [[Venom to pajak wsparcia, ktory paralizuje wrogie jednostki, aby inne jednostki mogly je bezpiecznie zniszczyc. Jest bardzo szybki jak na jednostke wsparcia.]],
	aimposoffset   = [[0 0 0]],
	midposoffset   = [[0 -6 0]],
	modelradius    = [[19]],
  },

  explodeAs              = [[BIG_UNITEX]],
  footprintX             = 3,
  footprintZ             = 3,
  iconType               = [[spiderriotspecial]],
  idleAutoHeal           = 5,
  idleTime               = 1800,
  leaveTracks            = true,
  maxDamage              = 750,
  maxSlope               = 72,
  maxVelocity            = 2.7,
  maxWaterDepth          = 22,
  minCloakDistance       = 75,
  movementClass          = [[TKBOT3]],
  noChaseCategory        = [[TERRAFORM FIXEDWING SATELLITE SUB]],
  objectName             = [[venom.s3o]],
  script                 = [[arm_venom.lua]],
  seismicSignature       = 4,
  selfDestructAs         = [[BIG_UNITEX]],

  sfxtypes               = {

    explosiongenerators = {
      [[custom:YELLOW_LIGHTNING_MUZZLE]],
      [[custom:YELLOW_LIGHTNING_GROUNDFLASH]],
    },

  },

  sightDistance          = 440,
  trackOffset            = 0,
  trackStrength          = 10,
  trackStretch           = 1,
  trackType              = [[ChickenTrackPointyShort]],
  trackWidth             = 54,
  turnRate               = 1600,

  weapons                = {

    {
      def                = [[spider]],
      onlyTargetCategory = [[SWIM LAND SINK TURRET FLOAT SHIP HOVER FIXEDWING GUNSHIP]],
    },

  },

  weaponDefs             = {

    spider = {
      name                    = [[Electro-Stunner]],
      areaOfEffect            = 160,
      collideFriendly         = false,
      craterBoost             = 0,
      craterMult              = 0,
	  
      customParams            = {
        extra_damage = [[18]],
        extra_damage_falloff_max = [[600]], -- make the extra damage proportional to (actual damage dealt)/extra_damage_falloff_max
		
		light_color = [[0.75 0.75 0.56]],
		light_radius = 190,
      },

      damage                  = {
        default        = 600.5,
      },

      duration                = 8,
      explosionGenerator      = [[custom:LIGHTNINGPLOSION160AoE]],
      fireStarter             = 0,
      heightMod               = 1,
      impulseBoost            = 0,
      impulseFactor           = 0,
      intensity               = 12,
      interceptedByShieldType = 1,
      noSelfDamage            = true,
      paralyzer               = true,
      paralyzeTime            = 3,
      range                   = 240,
      reloadtime              = 1.75,
      rgbColor                = [[1 1 0.7]],
      soundStart              = [[weapon/lightning_fire]],
      soundTrigger            = true,
      texture1                = [[lightning]],
      thickness               = 10,
      turret                  = true,
      weaponType              = [[LightningCannon]],
      weaponVelocity          = 450,
    },

  },

  featureDefs            = {

    DEAD  = {
      description      = [[Wreckage - Venom]],
      blocking         = false,
      damage           = 750,
      energy           = 0,
      featureDead      = [[HEAP]],
      footprintX       = 2,
      footprintZ       = 2,
      metal            = 80,
      object           = [[venom_wreck.s3o]],
      reclaimable      = true,
      reclaimTime      = 80,

    },
    HEAP  = {
      description      = [[Debris - Venom]],
      blocking         = false,
      damage           = 750,
      energy           = 0,
      footprintX       = 2,
      footprintZ       = 2,
      metal            = 40,
      object           = [[debris2x2a.s3o]],
      reclaimable      = true,
      reclaimTime      = 40,
    },

  },

}

return lowerkeys({ arm_venom = unitDef })
