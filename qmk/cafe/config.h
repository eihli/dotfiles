#ifndef CONFIG_H_
#define CONFIG_H_

#define NUM_ENCODERS 2
#define ENCODERS_A_PINS { F5, F4 }
#define ENCODERS_B_PINS { F5, F4 }
#define ENCODER_RESOLUTION 2

// Home row mods https://precondition.github.io/home-row-mods#what-are-home-row-mods
//
// Configure the global tapping term (default: 200ms)
#define TAPPING_TERM 175
// Enable rapid switch from tap to hold, disables double tap hold auto-repeat.
#define QUICK_TAP_TERM 120

#define BAS 0
#define SYM 1
#define NAV 2
#define NUM 3

#define MO_NAV  MO(NAV)
#define MO_SYM  MO(SYM)
#define MO_NUM  MO(NUM)
#define OSM_NAV OSM(NAV)
#define OSM_SYM OSM(SYM)
#define OSM_CMD OSM(SYM)

// Nav
#define WD_LEFT A(G(KC_LEFT))
#define WD_RGHT A(G(KC_RIGHT))

#endif // CONFIG_H_
