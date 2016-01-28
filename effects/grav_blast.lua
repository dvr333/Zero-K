-- grav_blast_seacloud_pillar
-- grav_blast_seacloud_topcap
-- grav_blast_seacloud_ring
-- grav_blast_landcloud_ring
-- grav_blast_seacloud
-- grav_blast_landcloud_pillar
-- grav_blast_effect
-- grav_blast_seacloud_cap
-- grav_blast_landcloud_cap
-- grav_blast_landcloud_topcap
-- grav_blast_landcloud

return {
  ["grav_blast_seacloud_pillar"] = {
    cloud = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.97,
        colormap           = [[0 0 0 0  0.8 0.8 1 1  0.8 0.8 1 0.75  0.8 0.8 1 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 0,
        emitrotspread      = 90,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 1,
        particlelife       = 240,
        particlelifespread = 40,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 1,
        particlespeedspread = 1,
        pos                = [[0, 0, 0]],
        sizegrowth         = 64,
        sizemod            = 0.75,
        texture            = [[smokesmall]],
      },
    },
  },

  ["grav_blast_seacloud_topcap"] = {
    cloud = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.97,
        colormap           = [[0 0 0 0  0.8 0.8 1 1  0.8 0.8 1 0.75  0.8 0.8 1 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 90,
        emitrotspread      = 5,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 4,
        particlelife       = 240,
        particlelifespread = 40,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 4,
        particlespeedspread = 4,
        pos                = [[0, 0, 0]],
        sizegrowth         = 64,
        sizemod            = 0.75,
        texture            = [[smokesmall]],
      },
    },
  },

  ["grav_blast_seacloud_ring"] = {
    cloud = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.97,
        colormap           = [[0 0 0 0  0.8 0.8 1 1  0.8 0.8 1 0.75  0.8 0.8 1 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 90,
        emitrotspread      = 5,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 128,
        particlelife       = 240,
        particlelifespread = 40,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 16,
        particlespeedspread = 1,
        pos                = [[0, 0, 0]],
        sizegrowth         = 32,
        sizemod            = 0.5,
        texture            = [[smokesmall]],
      },
    },
  },

  ["grav_blast_landcloud_ring"] = {
    land = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.97,
        colormap           = [[0 0 0 0  1 1 0.75 1  1 0.75 0.5 1  0.75 0.75 0.75 1  0 0 0 0]],
        directional        = false,
        emitrot            = 90,
        emitrotspread      = 5,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 128,
        particlelife       = 240,
        particlelifespread = 40,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 16,
        particlespeedspread = 1,
        pos                = [[0, 0, 0]],
        sizegrowth         = 32,
        sizemod            = 0.5,
        texture            = [[smokesmall]],
      },
    },
  },

  ["grav_blast_seacloud"] = {
    usedefaultexplosions = false,
    cap = {
      air                = true,
      class              = [[CExpGenSpawner]],
      count              = 96,
      ground             = true,
      water              = true,
      properties = {
        delay              = [[i1]],
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_SEACLOUD_CAP]],
        pos                = [[0, i8, 0]],
      },
    },
    pillar = {
      air                = true,
      class              = [[CExpGenSpawner]],
      count              = 128,
      ground             = true,
      water              = true,
      properties = {
        delay              = [[i1]],
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_SEACLOUD_PILLAR]],
        pos                = [[0, i8, 0]],
      },
    },
    ring = {
      air                = true,
      class              = [[CExpGenSpawner]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        delay              = 64,
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_SEACLOUD_RING]],
        pos                = [[0, 512, 0]],
      },
    },
    topcap = {
      air                = true,
      class              = [[CExpGenSpawner]],
      count              = 32,
      ground             = true,
      water              = true,
      properties = {
        delay              = [[96 i1]],
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_SEACLOUD_TOPCAP]],
        pos                = [[0, 768 i8, 0]],
      },
    },
  },

  ["grav_blast_landcloud_pillar"] = {
    land = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.97,
        colormap           = [[0 0 0 0  1 1 0.5 1  1 0.75 0.5 0.75  0.25 0.25 0.25 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 0,
        emitrotspread      = 90,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 1,
        particlelife       = 240,
        particlelifespread = 40,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 1,
        particlespeedspread = 1,
        pos                = [[0, 0, 0]],
        sizegrowth         = 64,
        sizemod            = 0.75,
        texture            = [[smokesmall]],
      },
    },
  },
  
  ["black_hole"] = {
    usedefaultexplosions = false,
    sparks = {
      air                = true,
	  ground             = true,
      water              = true,
	  underwater         = true,
      class              = [[CSimpleParticleSystem]],
      properties = {
        airdrag            = 1,
        colormap           = [[0 .8 1 .1   0 0 0 0]],
        directional        = true,
        emitrot            = 0,
        emitrotspread      = 120,
        emitvector         = [[0,1,0]],
        gravity            = [[0, 0, 0]],
        numparticles       = 20,
        particlelife       = 80,
        particlelifespread = 40,
        particlesize       = 110,
        particlesizespread = 110,
        particlespeed      = 0.15,
        particlespeedspread = 0.05,
        pos                = [[0, 10, 0]],
        sizegrowth         = -0.4,
        sizemod            = 1,
        texture            = [[chargeparticles]],
      },
    },
  },

  ["black_hole_singu"] = {
    usedefaultexplosions = false,
    sparks = {
      air                = true,
	  ground             = true,
      water              = true,
	  underwater         = true,
      class              = [[CSimpleParticleSystem]],
      properties = {
        airdrag            = 1,
        colormap           = [[0 .8 1 .1   0 0 0 0]],
        directional        = true,
        emitrot            = 0,
        emitrotspread      = 120,
        emitvector         = [[0,1,0]],
        gravity            = [[0, 0, 0]],
        numparticles       = 24,
        particlelife       = 120,
        particlelifespread = 70,
        particlesize       = 700,
        particlesizespread = 300,
        particlespeed      = 0.15,
        particlespeedspread = 0.05,
        pos                = [[0, 0, 0]],
        sizegrowth         = -1,
        sizemod            = 0.96,
        texture            = [[chargeparticles]],
      },
    },
  },

  ["black_hole_long"] = {
    usedefaultexplosions = false,
    sparks = {
      air                = true,
	  ground             = true,
      water              = true,
	  underwater         = true,
	  unit               = true,
      class              = [[CSimpleParticleSystem]],
      properties = {
        airdrag            = 1,
        colormap           = [[0 .8 1 .1   0 0 0 0]],
        directional        = true,
        emitrot            = 0,
        emitrotspread      = 120,
        emitvector         = [[0,1,0]],
        gravity            = [[0, 0, 0]],
        numparticles       = 20,
        particlelife       = 400,
        particlelifespread = 120,
        particlesize       = 110,
        particlesizespread = 110,
        particlespeed      = 0.15,
        particlespeedspread = 0.05,
        pos                = [[0, 10, 0]],
        sizegrowth         = -0.4,
        sizemod            = 1,
        texture            = [[chargeparticles]],
      },
    },
  },
  
  ["grav_danger_spikes_actual"] = {
    pikes = {
      air                = true,
      class              = [[explspike]],
      count              = 3,
      ground             = true,
      water              = true,
      underwater         = true,
      properties = {
        alpha              = 1,
        alphadecay         = 0.1,
        color              = [[1, 0.7, 0.2]],
        dir                = [[-55 r110,-55 r110,-55 r110]],
        length             = 32,
        width              = 50,
      },
    },
  },

  ["grav_danger_spikes"] = {
    nuke_600 = {
      air                = true,
      ground             = true,
      water              = true,
      underwater         = true,
      class              = [[CExpGenSpawner]],
      count              = 1,
      properties = {
        delay              = 0.5,
        dir                = [[dir]],
        explosiongenerator = [[custom:NUKE_600]],
        pos                = [[0, 20, 0]],
      },
    },
    spikes = {
      class              = [[CExpGenSpawner]],
      count              = 25,
      ground             = true,
      water              = true,
      underwater         = true,
      properties = {
        delay              = "i2",
        dir                = [[dir]],
        explosiongenerator = [[custom:grav_danger_spikes_actual]],
        pos                = [[0, 75, 0]],
      },
    },
    sphere = {
      air                = true,
      class              = [[CSpherePartSpawner]],
      count              = 1,
      ground             = true,
      water              = true,
      underwater         = true,
      properties = {
        alpha              = 0.3,
        color              = [[1, 0.7, 0.2]],
        expansionspeed     = 30,
        ttl                = 45,
      },
    },
    blackhole = {
      class              = [[CExpGenSpawner]],
      count              = 1,
      air                = true,
      ground             = true,
      water              = true,
      underwater         = true,
      properties = {
        delay              = 1,
        dir                = [[dir]],
        explosiongenerator = [[custom:black_hole_singu]],
        pos                = [[0, 75, 0]],
      },
    },
    --smoke = {
    --  class              = [[CExpGenSpawner]],
    --  count              = 1,
    --  ground             = true,
    --  water              = true,
    --  properties = {
    --    delay              = 50,
    --    dir                = [[dir]],
    --    explosiongenerator = [[custom:leveler_clouds_large]],
    --    pos                = [[0, 75, 0]],
    --  },
    --},
  },

  ["leveler_clouds_large"] = {
	dustcloud = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.85,
        colormap           = [[0.72 0.61 0.41 1      0 0 0 0.01]],
        directional        = false,
        emitrot            = 90,
        emitrotspread      = 10,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 30,
        particlelife       = 60,
        particlelifespread = 50,
        particlesize       = 7,
        particlesizespread = 3,
        particlespeed      = 3,
        particlespeedspread = 22,
        pos                = [[0, 10, 0]],
        sizegrowth         = 2,
        sizemod            = 1,
        texture            = [[smokesmall]],
      },
    },
  },
  
  ["grav_blast_effect"] = {
    usedefaultexplosions = false,
    groundflash = {
      circlealpha        = 1,
      circlegrowth       = 10,
      flashalpha         = 0.5,
      flashsize          = 1200,
      ttl                = 60,
      color = {
        [1]  = 1,
        [2]  = 0.5,
        [3]  = 0,
      },
    },
    landdirt = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      properties = {
        airdrag            = 0.95,
        colormap           = [[0 0 0 0  0.05 0.05 0.05 1  0.05 0.05 0.05 0.75  0 0 0 0 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 85,
        emitrotspread      = 5,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.025, 0]],
        numparticles       = 64,
        particlelife       = 120,
        particlelifespread = 20,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 2,
        particlespeedspread = 12,
        pos                = [[0, 0, 0]],
        sizegrowth         = 32,
        sizemod            = 0.75,
        texture            = [[dirt]],
      },
    },
    pikes = {
      air                = true,
      class              = [[explspike]],
      count              = 32,
      ground             = true,
      water              = true,
      properties = {
        alpha              = 0.2,
        alphadecay         = 0.001,
        color              = [[0.05, 0.05, 0.05]],
        dir                = [[-8 r16, -8 r16, -8 r16]],
        length             = 1,
        lengthgrowth       = 1,
        width              = 64,
      },
    },
    seacloud = {
      class              = [[CExpGenSpawner]],
      count              = 1,
      water              = true,
      properties = {
        delay              = 30,
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_SEACLOUD]],
        pos                = [[0, 0, 0]],
      },
    },
    sphere = {
      air                = true,
      class              = [[CSpherePartSpawner]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        alpha              = 0.5,
        color              = [[0.05, 0.05, 0.05]],
        expansionspeed     = 5,
        ttl                = 150,
      },
    },
    watermist = {
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      water              = true,
      properties = {
        airdrag            = 0.99,
        colormap           = [[0 0 0 0  0.8 0.8 1 1  0.8 0.8 1 0.75  0.8 0.8 1 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 0,
        emitrotspread      = 90,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, -0.05, 0]],
        numparticles       = 64,
        particlelife       = 80,
        particlelifespread = 20,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 12,
        particlespeedspread = 1,
        pos                = [[0, 0, 0]],
        sizegrowth         = 8,
        sizemod            = 1,
        texture            = [[smokesmall]],
      },
    },
  },

  ["grav_blast_seacloud_cap"] = {
    cloud = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.97,
        colormap           = [[0 0 0 0  0.8 0.8 1 1  0.8 0.8 1 0.75  0.8 0.8 1 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 90,
        emitrotspread      = 5,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 4,
        particlelife       = 30,
        particlelifespread = 20,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 4,
        particlespeedspread = 4,
        pos                = [[0, 0, 0]],
        sizegrowth         = 64,
        sizemod            = 0.75,
        texture            = [[smokesmall]],
      },
    },
  },

  ["grav_blast_landcloud_cap"] = {
    land = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.97,
        colormap           = [[0 0 0 0  1 1 0 1  1 1 1 0.75  0.25 0.25 0.25 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 90,
        emitrotspread      = 5,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 4,
        particlelife       = 30,
        particlelifespread = 20,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 4,
        particlespeedspread = 4,
        pos                = [[0, 0, 0]],
        sizegrowth         = 64,
        sizemod            = 0.75,
        texture            = [[fireball]],
      },
    },
  },

  ["grav_blast_landcloud_topcap"] = {
    land = {
      air                = true,
      class              = [[CSimpleParticleSystem]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        airdrag            = 0.97,
        colormap           = [[0 0 0 0  1 1 0 1  1 1 1 0.75  0.25 0.25 0.25 0.5  0 0 0 0]],
        directional        = false,
        emitrot            = 90,
        emitrotspread      = 5,
        emitvector         = [[0, 1, 0]],
        gravity            = [[0, 0.05, 0]],
        numparticles       = 4,
        particlelife       = 240,
        particlelifespread = 40,
        particlesize       = 4,
        particlesizespread = 4,
        particlespeed      = 4,
        particlespeedspread = 4,
        pos                = [[0, 0, 0]],
        sizegrowth         = 64,
        sizemod            = 0.75,
        texture            = [[fireball]],
      },
    },
  },

  ["grav_blast_landcloud"] = {
    usedefaultexplosions = false,
    cap = {
      air                = true,
      class              = [[CExpGenSpawner]],
      count              = 96,
      ground             = true,
      water              = true,
      properties = {
        delay              = [[i1]],
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_LANDCLOUD_CAP]],
        pos                = [[0, i8, 0]],
      },
    },
    pillar = {
      air                = true,
      class              = [[CExpGenSpawner]],
      count              = 128,
      ground             = true,
      water              = true,
      properties = {
        delay              = [[i1]],
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_LANDCLOUD_PILLAR]],
        pos                = [[0, i8, 0]],
      },
    },
    ring = {
      air                = true,
      class              = [[CExpGenSpawner]],
      count              = 1,
      ground             = true,
      water              = true,
      properties = {
        delay              = 64,
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_LANDCLOUD_RING]],
        pos                = [[0, 512, 0]],
      },
    },
    topcap = {
      air                = true,
      class              = [[CExpGenSpawner]],
      count              = 32,
      ground             = true,
      water              = true,
      properties = {
        delay              = [[96 i1]],
        dir                = [[dir]],
        explosiongenerator = [[custom:GRAV_BLAST_LANDCLOUD_TOPCAP]],
        pos                = [[0, 768 i8, 0]],
      },
    },
  },

}

