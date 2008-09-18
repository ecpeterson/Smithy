(* Resources.ml contains some pointers to some off-application resources, like
 * the glyphs used for the toolbar buttons.  These should eventually be stored
 * into a configuration file or something. *)

let arrowfile = "resources/arrow.png"
let linefile = "resources/line.png"
let polyfile = "resources/poly.png"
let fillfile = "resources/fill.png"
let panfile = "resources/pan.png"
let zoomfile = "resources/zoom.png"
let textfile = "resources/text.png"
let objfile = "resources/obj.png"

let sceneryfile = "resources/flower.png"
let goalfile = "resources/flag.png"
let sound_sourcefile = "resources/sound.png"

let preferences_file = (Sys.getenv "HOME") ^ "/.smithy"

let item_hash = Hashtbl.create 36
let mk_item name file =
    Hashtbl.add item_hash name ("resources/" ^ file ^ ".png")
let _ =
    mk_item "Knife"                   "";
    mk_item "Magnum"                  "pistol";
    mk_item "Magnum Clip"             "pistol-ammo";
    mk_item "Fusion Pistol"           "fusion";
    mk_item "Fusion Battery"          "fusion-ammo";
    mk_item "Assault Rifle"           "ar";
    mk_item "Assault Rifle Clip"      "ar-ammo";
    mk_item "Grenades"                "ar-grenades";
    mk_item "Missile Launcher"        "rl";
    mk_item "Missile 2-pack"          "rl-ammo";
    mk_item "Invisibility Powerup"    "powerup";
    mk_item "Invincibility Powerup"   "invinc";
    mk_item "Infravision Powerup"     "brown-chip";
    mk_item "Alien Weapon"            "alien-gun";
    mk_item "Alien Weapon Ammunition" "";
    mk_item "Napalm Launcher"         "tozt";
    mk_item "Napalm Canister"         "tozt-ammo";
    mk_item "Extravision Powerup"     "red-chip";
    mk_item "Oxygen Powerup"          "oxygen";
    mk_item "Energy Powerup x1"       "1x";
    mk_item "Energy Powerup x2"       "2x";
    mk_item "Energy Powerup x3"       "3x";
    mk_item "Shotgun"                 "shotgun";
    mk_item "Shotgun Cartridge"       "shotgun-ammo";
    mk_item "S'pht Door Key"          "keycard";
    mk_item "Uplink Chip"             "uplink-chip";
    mk_item "Slate Ball"              "skull";
    mk_item "Red Ball (the skull)"    "skull";
    mk_item "Violet Ball"             "skull";
    mk_item "Yellow Ball"             "skull";
    mk_item "White Ball"              "skull";
    mk_item "Orange Ball"             "skull";
    mk_item "Blue Ball"               "skull";
    mk_item "Green Ball"              "skull";
    mk_item "Sub-Machine Gun"         "smg";
    mk_item "SMG Ammo"                "smg-ammo"

let warning = "/!\\ Roof Notification /!\\"
