#include QMK_KEYBOARD_H
#if __has_include("keymap.h")
#    include "keymap.h"
#endif

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [0] = LAYOUT(
                 XXXXXXX,    KC_1,       KC_2,     KC_3,      KC_4,      KC_5,                              KC_6,       KC_7,       KC_8,    KC_9,    KC_0,       KC_GRV,
                 KC_TAB,     KC_Q,       KC_W,     KC_E,      KC_R,      KC_T,                              KC_Y,       KC_U,       KC_I,    KC_O,    KC_P,       KC_MINS,
                 KC_LCTL,    KC_A,       HOME_S,   HOME_D,    HOME_F,    KC_G,                              KC_H,       HOME_J,     HOME_K,  HOME_L,  KC_SCLN,    KC_RCTL,
                 KC_LSFT,    KC_Z,       KC_X,     KC_C,      KC_V,      KC_B,     XXXXXXX,    XXXXXXX,     KC_N,       KC_M,       KC_COMM, KC_DOT,  KC_QUOT,    KC_RSFT,
                                         KC_LGUI,  KC_LALT,   MO(1),     KC_SPC,                            KC_ENT,     MO(2),      KC_LALT, KC_RGUI
                ),
    [1] = LAYOUT(
                 KC_F1,      KC_F2,      KC_F3,      KC_F4,      KC_F5,      KC_F6,                              KC_F7,      KC_F8,      KC_F9,      KC_F10,     KC_F11,     KC_F12,
                 KC_CAPS,    CW_TOGG,    KC_ESC,     KC_QUOT,    KC_DQUO,    KC_TAB,                             KC_SLSH,    KC_BSLS,    KC_EQL,     KC_LBRC,    KC_RBRC,    KC_DEL,
                 _______,    KC_EXLM,    KC_AT,      KC_HASH,    KC_DLR,     KC_PERC,                            KC_CIRC,    KC_AMPR,    KC_ASTR,    KC_LPRN,    KC_RPRN,    _______,
                 _______,    XXXXXXX,    KC_TAB,     XXXXXXX,    XXXXXXX,    KC_LGUI,    XXXXXXX,    XXXXXXX,    KC_RGUI,    KC_MINS,    KC_UNDS,    KC_LCBR,    KC_RCBR,    _______,
                                         _______,    _______,    _______,    _______,                            KC_BSPC,    MO(3),      _______,    _______
                 ),
    [2] = LAYOUT(
                 XXXXXXX,    XXXXXXX,    XXXXXXX,    XXXXXXX,    XXXXXXX,    XXXXXXX,                            XXXXXXX,    XXXXXXX,    XXXXXXX,    XXXXXXX,    XXXXXXX,    XXXXXXX,
                 XXXXXXX,    KC_1,       KC_2,       KC_3,       KC_4,       KC_5,                               KC_6,       KC_7,       KC_8,       KC_9,       KC_0,       XXXXXXX,
                 _______,    KC_HOME,    KC_END,     KC_PGUP,    KC_PGDN,    XXXXXXX,                            KC_TAB,     KC_LEFT,    KC_DOWN,    KC_UP,      KC_RGHT,    _______,
                 _______,    KC_F8,      KC_F9,      KC_F10,     KC_F11,     _______,    XXXXXXX,    XXXXXXX,    _______,    XXXXXXX,    _______,    _______,    XXXXXXX,    _______,
                                         _______,    _______,    MO(3),      KC_TAB,                             _______,    _______,    _______,    _______
                ),
    [3] = LAYOUT(
                 XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
                 XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
                 XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                   XXXXXXX, XXXXXXX, RM_TOGG, RM_HUEU, RM_SATU, RM_VALU,
                 XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, RM_NEXT, RM_HUED, RM_SATD, RM_VALD,
                                   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
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
