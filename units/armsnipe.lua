unitDef = {
  unitname               = [[armsnipe]],
  name                   = [[Spectre]],
  description            = [[Cloaked Skirmish/Anti-Heavy Artillery Bot]],
  acceleration           = 0.3,
  brakeRate              = 0.2,
  buildCostEnergy        = 750,
  buildCostMetal         = 750,
  buildPic               = [[ARMSNIPE.png]],
  buildTime              = 750,
  canAttack              = true,
  canGuard               = true,
  canMove                = true,
  canPatrol              = true,
  canstop                = [[1]],
  category               = [[LAND]],
  cloakCost              = 1,
  cloakCostMoving        = 5,
  collisionVolumeOffsets = [[0 0 0]],
  collisionVolumeScales  = [[30 60 30]],
  collisionVolumeType    = [[cylY]],
  corpse                 = [[DEAD]],

  customParams           = {
    description_bp = [[Robô Sniper]],
    description_de = [[Scharfschützen Walker (Skirmish/Anti-Heavy)]],
    description_es = [[Caminante tirador]],
    description_fi = [[Tarkka-ampujarobotti]],
    description_fr = [[Marcheur Sniper]],
    description_it = [[Camminatore Cecchino]],
    helptext       = [[The Spectre's energy rifle inflicts heavy damage to a single target. It can fire while cloaked; however its visible round betrays its position. It requires quite a bit of energy to keep cloaked, especially when moving. The best way to locate a Spectre is by sweeping the area with many cheap units.]],
    helptext_bp    = [[Spectre ? uma unidade de artilharia invis?vel a radar e capaz de se camuflar. Pode atirar enquanto camuflado, mas seus tiros vis?veis indicam sua posi?ao. Requer muita energia para manter camuflado e atirando. Quando destru?do, uma onda de PEM paraliza unidades pr?ximas. Usado melhor s?zinho.]],
    helptext_de    = [[Sein energetisches Gewehr richtet riesigen Schaden bei einzelnen Zielen an. Er kann auch schießen, wenn er getarnt ist. Dennoch verrät ihn sein sichtbarer Schuss. Um getarnt zu bleiben und schießen zu können, benötigt der Scharfschütze eine Menge Energie. Die einfachst Möglichkeit einen Scharfschützen ausfindig zu machen, ist die, indem man ein Gebiet mit vielen billigen überschwemmt.]],
    helptext_es    = [[El Spectre es una unidad ocultada de artilleria costosa. Puede disparar mientras ocultado, pero la bala visible revela su posición. Necesita mucha energía para disparar mientras ocultado. La mejor manera de encontrar un Spectre es de ispeccionar el área con unidades de exploración.]],
    helptext_fi    = [[Kallis, vihollisen tutkaan ilmaantumaton ja n?kym?tt?myyskentt?? hy?dynt?v? Spectre on tehokas erikoisteht?vien yksikk?. Specterin korkeaenergisen, pitk?n kantaman pulssiplasma-aseen k?ytt?minen sek? n?kym?tt?m?n? pit?minen vaativat paljon energiaa. Tehokkain tapa paikantaa t?m? yksikk? on l?hett?? lukuisia tiedustelijoita alueelle.]],
    helptext_fr    = [[Le Spectre est une unit? d'artillerie furtive, camouflable et coutant tres cher. Il peut faire feu tout en restant camoufl?. Son tir tres visible peut cependant r?veler sa position. La quantit?e d'?nergie qu'il n?cessite pour tirer et rester camoufler en m?me temps est ?lev?e. Sa destruction ?met une onde de choque EMP qui immobilise les unit?s qui se trouve a proximit?. Il est le plus utile en tant que tireur isol?.]],
    helptext_it    = [[Il Spectre é un unita costosa occultata d'artiglieria. Puo sparare mentre é occultata; pero il proiettile visibile tradisce la sua posizione. Richiede molta energia per tenerlo occultato mentre spara. la migliore maniera di trovare un Spectre é di ispezionare l'area con unita di ricognizione]],
	modelradius    = [[15]],
	dontfireatradarcommand = '1',
  },

  decloakOnFire          = false,
  explodeAs              = [[BIG_UNITEX]],
  footprintX             = 2,
  footprintZ             = 2,
  iconType               = [[sniper]],
  idleAutoHeal           = 5,
  idleTime               = 1800,
  leaveTracks            = true,
  losEmitHeight          = 40,
  initCloaked            = true,
  maxDamage              = 560,
  maxSlope               = 36,
  maxVelocity            = 1.45,
  maxWaterDepth          = 22,
  minCloakDistance       = 155,
  movementClass          = [[KBOT2]],
  moveState              = 0,
  noChaseCategory        = [[TERRAFORM FIXEDWING GUNSHIP SUB TOOFAST CHEAP]],
  objectName             = [[sharpshooter.s3o]],
  script                 = [[armsnipe.lua]],
  seismicSignature       = 16,
  selfDestructAs         = [[BIG_UNITEX]],

  sfxtypes               = {

    explosiongenerators = {
      [[custom:WEAPEXP_PUFF]],
      [[custom:MISSILE_EXPLOSION]],
    },

  },

  sightDistance          = 400,
  trackOffset            = 0,
  trackStrength          = 8,
  trackStretch           = 1,
  trackType              = [[ComTrack]],
  trackWidth             = 22,
  turnRate               = 2600,
  upright                = true,

  weapons                = {

    {
      def                = [[SHOCKRIFLE]],
      badTargetCategory  = [[FIXEDWING]],
      onlyTargetCategory = [[FIXEDWING LAND SINK TURRET SHIP SWIM FLOAT GUNSHIP HOVER]],
    },

  },

  weaponDefs             = {

    SHOCKRIFLE = {
      name                    = [[Pulsed Particle Projector]],
      areaOfEffect            = 16,
      colormap                = [[0 0 0.4 0   0 0 0.6 0.3   0 0 0.8 0.6   0 0 0.9 0.8   0 0 1 1   0 0 1 1]],
      craterBoost             = 0,
      craterMult              = 0,

	  customParams        	  = {
		light_radius = 0,
	  },
	  
      damage                  = {
        default = 1500.1,
        planes  = 1500.1,
        subs    = 75,
      },

      explosionGenerator      = [[custom:megapartgun]],
      impactOnly              = true,
      impulseBoost            = 0,
      impulseFactor           = 0.4,
      interceptedByShieldType = 1,
      noSelfDamage            = true,
      range                   = 700,
      reloadtime              = 17,
      rgbColor                = [[1 0.2 0.2]],
      separation              = 1.5,
      size                    = 5,
      sizeDecay               = 0,
      soundHit                = [[weapon/laser/heavy_laser6]],
      soundStart              = [[weapon/gauss_fire]],
      turret                  = true,
      weaponType              = [[Cannon]],
      weaponVelocity          = 850,
    },

  },

  featureDefs            = {

    DEAD = {
      description      = [[Wreckage - Spectre]],
      blocking         = true,
      damage           = 560,
      energy           = 0,
      featureDead      = [[HEAP]],
      footprintX       = 2,
      footprintZ       = 2,
      metal            = 300,
      object           = [[sharpshooter_dead.s3o]],
      reclaimable      = true,
      reclaimTime      = 300,
    },

    HEAP = {
      description      = [[Debris - Spectre]],
      blocking         = false,
      damage           = 560,
      energy           = 0,
      footprintX       = 2,
      footprintZ       = 2,
      metal            = 150,
      object           = [[debris2x2b.s3o]],
      reclaimable      = true,
      reclaimTime      = 150,
    },

  },

}

return lowerkeys({ armsnipe = unitDef })
