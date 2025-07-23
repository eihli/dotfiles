#include QMK_KEYBOARD_H
#if __has_include("keymap.h")
#    include "keymap.h"
#endif

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [BAS] = LAYOUT(
                 XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,                      XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,
                 XXXXXXX,  KC_Q   ,  KC_W   ,  KC_E   ,  KC_R   ,  KC_T   ,                      KC_Y   ,  KC_U   ,  KC_I   ,  KC_O   ,  KC_P   ,  XXXXXXX,
                 XXXXXXX,  KC_A   ,  KC_S   ,  KC_D   ,  KC_F   ,  KC_G   ,                      KC_H   ,  KC_J   ,  KC_K   ,  KC_L   ,  KC_SCLN,  XXXXXXX,
                 XXXXXXX,  KC_Z   ,  KC_X   ,  KC_C   ,  KC_V   ,  KC_B   ,  XXXXXXX,  XXXXXXX,  KC_N   ,  KC_M   ,  KC_COMM,  KC_DOT ,  KC_SLSH,  XXXXXXX,
                                     XXXXXXX,  XXXXXXX,  MO_NAV ,  KC_TAB ,                      KC_SPC ,  MO_SYM ,  XXXXXXX,  XXXXXXX
                ),
    [SYM] = LAYOUT(
                 XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,                      XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,
                 XXXXXXX,  KC_QUOT,  KC_LBRC,  KC_LCBR,  KC_LPRN,  KC_TILD,                      KC_CIRC,  KC_RPRN,  KC_RCBR,  KC_RBRC,  KC_GRV ,  XXXXXXX,
                 XXXXXXX,  KC_MINS,  KC_ASTR,  KC_EQL ,  KC_UNDS,  KC_DLR ,                      KC_HASH,  OS_RSFT,  OS_LALT,  OS_RCTL,  OS_RGUI,  XXXXXXX,
                 XXXXXXX,  KC_PLUS,  KC_PIPE,  KC_AT  ,  KC_SLSH,  KC_PERC,  XXXXXXX,  XXXXXXX,  KC_DQUO,  KC_BSLS,  KC_AMPR,  KC_QUES,  KC_EXLM,  XXXXXXX,
                                     XXXXXXX,  XXXXXXX,  MO_NUM ,  _______,                      _______,  _______,  XXXXXXX,  XXXXXXX
                 ),
    [NAV] = LAYOUT(
                 XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,                      XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,
                 XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,                      KC_HOME,  KC_PGUP,  KC_PGDN,  KC_END ,  KC_DEL ,  XXXXXXX,
                 XXXXXXX,  OS_LGUI,  OS_LCTL,  OS_LALT,  OS_LSFT,  KC_CAPS,                      KC_LEFT,  KC_DOWN,  KC_UP  ,  KC_RGHT,  KC_BSPC,  XXXXXXX,
                 XXXXXXX,  WD_LEFT,  WD_RGHT,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  CW_TOGG,  XXXXXXX,  XXXXXXX,  XXXXXXX,  KC_ESC,  XXXXXXX,
                                     XXXXXXX,  XXXXXXX,  _______,  _______,                      KC_ENT ,  MO_NUM ,  XXXXXXX,  XXXXXXX
                ),
    [NUM] = LAYOUT(
                 XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,                      XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,  XXXXXXX,
                 XXXXXXX,  KC_1   ,  KC_2   ,  KC_3   ,  KC_4   ,  KC_5   ,                      KC_6   ,  KC_7   ,  KC_8   ,  KC_9   ,  KC_0   ,  XXXXXXX,
                 XXXXXXX,  OS_LGUI,  OS_LCTL,  OS_LALT,  OS_LSFT,  KC_F11 ,                      KC_F12 ,  OS_RSFT,  OS_LALT,  OS_RCTL,  OS_RGUI,  XXXXXXX,
                 XXXXXXX,  KC_F1  ,  KC_F2  ,  KC_F3  ,  KC_F4  ,  KC_F5  ,  XXXXXXX,  XXXXXXX,  KC_F6  ,  KC_F7  ,  KC_F8  ,  KC_F9  ,  KC_F10 ,  XXXXXXX,
                                     XXXXXXX,  XXXXXXX,  _______,  _______,                      _______,  _______,  XXXXXXX,  XXXXXXX
                )
};


#if defined(ENCODER_MAP_ENABLE)
const uint16_t PROGMEM encoder_map[][NUM_ENCODERS][NUM_DIRECTIONS] = {
    [0] = { ENCODER_CCW_CW(MS_WHLU, MS_WHLD),  ENCODER_CCW_CW(KC_VOLD, KC_VOLU)  },
    [1] = { ENCODER_CCW_CW(UG_HUED, UG_HUEU),  ENCODER_CCW_CW(UG_SATD, UG_SATU)  },
    [2] = { ENCODER_CCW_CW(UG_VALD, UG_VALU),  ENCODER_CCW_CW(UG_SPDD, UG_SPDU)  },
    [3] = { ENCODER_CCW_CW(UG_PREV, UG_NEXT),  ENCODER_CCW_CW(KC_RIGHT, KC_LEFT) },
};
#endif

bool caps_word_press_user(uint16_t keycode) {
    switch (keycode) {
        // Keycodes that continue Caps Word, with shift applied.
        case KC_A ... KC_Z:
        case KC_MINS:
            add_weak_mods(MOD_BIT(KC_LSFT));  // Apply shift to next key.
            return true;

        // Keycodes that continue Caps Word, without shifting.
        case KC_1 ... KC_0:
        case KC_BSPC:
        case KC_DEL:
        case KC_SCLN:  // Norman turns semi-colon into ASCII h.
        case KC_UNDS:
            return true;

        default:
            return false;  // Deactivate Caps Word.
    }
}
