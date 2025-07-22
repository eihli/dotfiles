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

// Left-hand home row mods (Norman layout)
#define HOME_F LCTL_T(KC_F)
#define HOME_D LALT_T(KC_D)
#define HOME_S LSFT_T(KC_S)
#define HOME_B LGUI_T(KC_B)

// Left-hand layer 2
#define LSFTAT   MT(MOD_LSFT, KC_AT)
#define LALTHASH LALT_T(KC_HASH)
#define LCTLDLR  LCTL_T(KC_DLR)

// Left-hand layer 3
#define LSFTEND  LSFT_T(KC_END)
#define LALTPGUP LALT_T(KC_PGUP)
#define LCTLPGDN LCTL_T(KC_PGDN)

// Right-hand home row mods
#define HOME_J RCTL_T(KC_J)
#define HOME_K LALT_T(KC_K)
#define HOME_L RSFT_T(KC_L)
#define HOME_N LGUI_T(KC_N)

// Right-hand layer 2
#define RCTLAMPR RCTL_T(KC_AMPR)
#define RALTASTR RALT_T(KC_ASTR)
#define RSFTLPRN RSFT_T(KC_LPRN)

// Right-hand layer 3
#define RCTLLEFT RCTL_T(KC_LEFT)
#define RALTDOWN LALT_T(KC_DOWN)
#define RSFTUP RSFT_T(KC_UP)

#endif // CONFIG_H_
