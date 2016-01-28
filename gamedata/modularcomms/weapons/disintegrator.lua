local name = "commweapon_disintegrator"
local weaponDef = {
	name                    = [[Disintegrator]],
	areaOfEffect            = 48,
	avoidFeature            = false,
	avoidFriendly           = false,
	avoidNeutral            = false,
	commandfire             = true,
	craterBoost             = 1,
	craterMult              = 6,

	customParams            = {
		muzzleEffectShot = [[custom:ataalaser]],
		slot = [[3]],
		manualfire = 1,
	},

	damage                  = {
		default    = 1400,
	},

	explosionGenerator      = [[custom:DGUNTRACE]],
	impulseBoost            = 0,
	impulseFactor           = 0,
	interceptedByShieldType = 0,
	noExplode               = true,
	noSelfDamage            = true,
	range                   = 200,
	reloadtime              = 20,
	size                    = 6,
	soundHit                = [[explosion/ex_med6]],
	soundStart              = [[weapon/laser/heavy_laser4]],
	soundTrigger            = true,
	turret                  = true,
	weaponType              = [[DGun]],
	weaponVelocity          = 300,
}

return name, weaponDef
