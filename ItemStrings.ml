(*** ItemStrings.ml contains various interface strings. ***)

let monster_strings =
    ["Tick (energy)"; "Tick (oxygen)"; "Tick (kamikaze)"; "S'pht (minor)";
     "S'pht (major)"; "S'pht (minor invisible)"; "S'pht (major invisible)";
     "Pfhor Fighter (minor)"; "Pfhor Fighter (major)";
     "Pfhor Fighter (minor projectile)"; "Pfhor Fighter (major projectile)";
     "Bob (crew)"; "Bob (science)"; "Bob (security)"; "Bob (assimilated)";
     "Drone (minor)"; "Drone (major)"; "Drone (big minor)"; "Drone (big major)";
     "Drone (Durandal)"; "Cyborg (minor)"; "Cyborg (major)";
     "Cyborg (minor flame)"; "Cyborg (major flame)"; "Enforcer (minor)";
     "Enforcer (major)"; "Hunter (minor)"; "Hunter (major)"; "Trooper (minor)";
     "Trooper (major)"; "Mother of All Cyborgs"; "Mother of All Hunters";
     "F'lickta (sewage)"; "F'lickta (water)"; "F'lickta (lava)";
     "S'pht'kr (minor)"; "S'pht'kr (major)"; "Juggernaut (minor)";
     "Juggernaut (major)"; "Tiny Fighter"; "Tiny Bob"; "Tiny F'lickta";
     "VacBob (crew)"; "VacBob (science)"; "VacBob (security)";
     "VacBob (assimilated)"]
(*
    public static final int LAVA_START = 0;
    public static final int LAVA_END = 12;
    public static final int WATER_START = 13;
    public static final int WATER_END = 27;
    public static final int SEWAGE_START = 28;
    public static final int SEWAGE_END = 38;
    public static final int PFHOR_START = 39;
    public static final int PFHOR_END = 49;
    public static final int JJARO_START = 50;
    public static final int JJARO_END = 60;
                                                *)
let scenery_strings_lava =
    ["Light Dirt"; "Dark Dirt"; "Bones"; "Bone"; "Ribs"; "Skull";
     "Hanging Light #1"; "Hanging Light #2"; "Large Cylinder"; "Small Cylinder";
     "Block #1"; "Block #2"; "Block #3"]
let scenery_strings_water =
    ["Pistol Clip"; "Short Light"; "Long Light"; "Siren"; "Rocks";
     "Blood Puddles"; "Water Filtration Device"; "Bloody Gun"; "Bloody Stuff";
     "Puddles"; "Big Puddles"; "Security Monitor"; "Alien Trash Can";
     "Machine"; "Fighter's Staff"]
let scenery_strings_sewage =
    ["Stubby Greeb Light"; "Long Green Light"; "Junk"; "Big Antenna";
     "Small Antenna"; "Alien Trashcan"; "Bones"; "Big Bones"; "Pfhor Pieces";
     "Bob Pieces"; "Bob Blood"]
let scenery_strings_pfhor =
    ["Green Light"; "Small Alien Light"; "Alien Ceiling Light";
     "Bulbous Yellow Object"; "Square Grey Organic Object"; "Pfhor Skeleton";
     "Pfhor Mask"; "Green Stuff"; "Hunter Shield"; "Bones"; "Alien Sludge"]
let scenery_strings_jjaro =
    ["Short Ceiling Light"; "Tall Light"; "Weird Rod"; "Pfhor Ship"; "Sun";
     "Large Glass Container"; "Nub 1"; "Nub 2"; "Lh'owon"; "Floor Whip Antenna";
     "Ceiling Whip Antenna"]

let item_strings =
    ["Knife"; "Magnum"; "Magnum Clip"; "Fusion Pistol"; "Fusion Battery";
     "Assault Rifle"; "Assault Rifle Clip"; "Grenades"; "Missile Launcher";
     "Missile 2-pack"; "Invisibility Powerup"; "Invincibility Powerup";
     "Infravision Powerup"; "Alien Weapon"; "Alien Weapon Ammunition";
     "Napalm Launcher"; "Napalm Canister"; "Extravision Powerup";
     "Oxygen Powerup"; "Energy Powerup x1"; "Energy Powerup x2";
     "Energy Powerup x3"; "Shotgun"; "Shotgun Cartridge"; "S'pht Door Key";
     "Uplink Chip"; "Slate Ball"; "Red Ball (the skull)"; "Violet Ball";
     "Yellow Ball"; "White Ball"; "Orange Ball"; "Blue Ball"; "Green Ball";
     "Sub-Machine Gun"; "SMG Ammo"]

let player_strings =
    ["Slate Team"; "Red Team"; "Violet Team"; "Yellow Team";
    "White Team"; "Orange Team"; "Blue Team"; "Green Team"]

let sound_strings =
    ["Water"; "Sewage"; "Lava"; "Goo"; "Under Media"; "Wind"; "Waterfall";
     "Siren"; "Fan"; "S'pht Door"; "S'pht Platform"; "Heavy S'pht Door";
     "Heavy S'pht Platform"; "Light Machinery"; "Heavy Machinery"; "Tranformer";
     "Sparking Transformer"; "Machine Binder"; "Machine Bookpress";
     "Machine Puncher"; "Electric Hum"; "Siren"; "Night Wind"; "Pfhor Door";
     "Pfhor Platform"; "Pfhor Ship #1"; "Pfhor Ship #2"; "Jjaro Ship"]

let random_sound_strings =
    ["Dripping Water"; "Thunder"; "Underground Explosions";
     "Lh'owon Loon"; "Jjaro Ship Creak"]

let landscape_strings = ["Daytime Lh'owon"; "Nighttime Lh'owon";
                         "Moon"; "Space"]
