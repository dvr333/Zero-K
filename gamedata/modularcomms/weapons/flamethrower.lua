local name = "commweapon_flamethrower"
local weaponDef = {
	name                    = [[Flame Thrower]],
	areaOfEffect            = 64,
	avoidGround             = false,
	avoidFeature            = false,
	cegTag                  = [[flamer]],
	collideFeature          = false,
	collideGround           = false,
	craterBoost             = 0,
	craterMult              = 0,

	customParams            = {
		slot = [[5]],
		muzzleEffectFire = [[custom:RAIDMUZZLE]],
		flamethrower = [[1]],
		setunitsonfire = "1",
		burntime = [[450]],
		
		light_camera_height = 2800,
		light_color = [[1.2 0.78 0.36]],
		light_radius = 260,
	},

	damage                  = {
		default = 10,
		subs    = 0.1,
	},

	explosionGenerator      = [[custom:SMOKE]],
	fallOffRate             = 1,
	fireStarter             = 100,
	impulseBoost            = 0,
	impulseFactor           = 0,
	interceptedByShieldType = 1,
	noExplode               = true,
	noSelfDamage            = true,
	--predictBoost            = 1,
	range                   = 270,
	reloadtime              = 5/30,
	rgbColor                = [[1 1 1]],
	soundStart              = [[weapon/flamethrower]],
	soundTrigger            = true,
    texture1				= [[flame]],
    thickness				= 0,
	tolerance               = 5000,
	turret                  = true,
	weaponType              = [[LaserCannon]],
	weaponVelocity          = 800,
}

return name, weaponDef
