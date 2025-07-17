#include QMK_KEYBOARD_H
#if __has_include("keymap.h")
#    include "keymap.h"
#endif

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [0] = LAYOUT(
                 KC_ESC,     KC_1,       KC_2,     KC_3,      KC_4,      KC_5,                              KC_6,       KC_7,       KC_8,    KC_9,    KC_0,       KC_GRV,
                 KC_TAB,     KC_Q,       KC_W,     KC_E,      KC_R,      KC_T,                              KC_Y,       KC_U,       KC_I,    KC_O,    KC_P,       KC_BSPC,
                 KC_LCTL,    KC_A,       KC_S,     KC_D,      KC_F,      KC_G,                              KC_H,       KC_J,       KC_K,    KC_L,    KC_SCLN,    KC_RCTL,
                 KC_LSFT,    KC_Z,       KC_X,     KC_C,      KC_V,      KC_B,     _______,    _______,     KC_N,       KC_M,       KC_COMM, KC_DOT,  KC_SLSH,    KC_RSFT,
                                         KC_LGUI,  KC_LALT,   MO(1),     KC_SPC,                            KC_ENT,     MO(2),      KC_RALT, KC_RGUI
                ),
    [1] = LAYOUT(
                 KC_F1,      KC_F2,      KC_F3,      KC_F4,      KC_F5,      KC_F6,                              KC_F7,      KC_F8,      KC_F9,      KC_F10,     KC_F11,     KC_F12,
                 _______,    _______,    _______,    KC_QUOT,    KC_DQUO,    _______,                            _______,    KC_PLUS,    KC_EQL,     KC_LBRC,    KC_RBRC,    KC_DEL,
                 QK_BOOT,    KC_EXLM,    KC_AT,      KC_HASH,    KC_DLR,     KC_PERC,                            KC_CIRC,    KC_AMPR,    KC_ASTR,    KC_LPRN,    KC_RPRN,    _______,
                 _______,    _______,    _______,    _______,    _______,    _______,    _______,    _______,    XXXXXXX,    KC_MINS,    KC_UNDS,    KC_LCBR,    KC_RCBR,    _______,
                                         _______,    _______,    _______,    _______,                            _______,    MO(3),      _______,    _______
                 ),
    [2] = LAYOUT(
                 _______,    _______,    _______,    _______,    _______,    _______,                            _______,    _______,    _______,    _______,    _______,    _______,
                 _______,    KC_1,       KC_2,       KC_3,       KC_4,       KC_5,                               KC_6,       KC_7,       KC_8,       KC_9,       KC_0,       _______,
                 KC_CAPS,    KC_HOME,    KC_END,     KC_PGUP,    KC_PGDN,    KC_F5,                              XXXXXXX,    KC_LEFT,    KC_DOWN,    KC_UP,      KC_RGHT,    XXXXXXX,
                 KC_F7,      KC_F8,      KC_F9,      KC_F10,     KC_F11,     KC_F12,     _______,    _______,    KC_PLUS,    KC_MINS,    KC_EQL,     KC_LBRC,    KC_RBRC,    KC_BSLS,
                                         _______,    _______,    MO(3),      _______,                            _______,    _______,    _______,    _______
                ),
    [3] = LAYOUT(
                 XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
                 XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
                 XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                   XXXXXXX, XXXXXXX, RM_TOGG, RM_HUEU, RM_SATU, RM_VALU,
                 XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, RM_NEXT, RM_HUED, RM_SATD, RM_VALD,
                                   _______, _______, _______, _______,                   _______, _______, _______, _______
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
