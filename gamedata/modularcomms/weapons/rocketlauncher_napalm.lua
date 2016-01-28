local name = "commweapon_rocketlauncher_napalm"
local weaponDef = {
	name                    = [[Napalm Rocket Launcher]],
	areaOfEffect            = 128,
	cegTag                  = [[missiletrailred]],
	craterBoost             = 0,
	craterMult              = 0,

	customParams            = {
		slot = [[5]],
		muzzleEffectFire = [[custom:STORMMUZZLE]],
		burntime         = 450,
		burnchance       = 1,
		setunitsonfire   = [[1]],
	},

	damage                  = {
		default = 360,
		planes  = 360,
		subs    = 18,
	},
  
	explosiongenerator      = [[custom:napalm_phoenix]],
	fireStarter             = 180,
	flightTime              = 3,
	impulseBoost            = 0,
	impulseFactor           = 0.4,
	interceptedByShieldType = 2,
	model                   = [[wep_m_hailstorm.s3o]],
	predictBoost            = 1,
	range                   = 430,
	reloadtime              = 3,
	smokeTrail              = true,
	soundHit                = [[weapon/burn_mixed]],
	soundHitVolume          = 7,
	soundStart              = [[weapon/missile/sabot_fire]],
	soundStartVolume        = 7,
	startVelocity           = 200,
	texture2                = [[darksmoketrail]],
	tracks                  = false,
	trajectoryHeight        = 0.05,
	turret                  = true,
	weaponAcceleration      = 100,
	weaponType              = [[MissileLauncher]],
	weaponVelocity          = 250,
}

return name, weaponDef
