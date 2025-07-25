import Principal "mo:base/Principal";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Result "mo:base/Result";
import Option "mo:base/Option";
import Char "mo:base/Char";

actor UserManager {
    // Types
    public type UserProfile = {
        principal : Principal;
        name : Text;
        bio : Text;
        avatar : Text;
        isCreator : Bool;
        totalEarnings : Nat;
        createdAt : Int;
        followersCount : Nat;
        followingCount : Nat;
    };

    // Correct hash function for Nat keys
    private let hashNat = func(n : Nat) : Nat32 {
        Nat32.fromNat(n);
    };

    // State variables with correct hash functions
    private var profiles = HashMap.HashMap<Principal, UserProfile>(100, Principal.equal, Principal.hash);
    private var usernames = HashMap.HashMap<Text, Principal>(100, Text.equal, Text.hash);
    private var followers = HashMap.HashMap<Principal, [Principal]>(100, Principal.equal, Principal.hash);
    private var following = HashMap.HashMap<Principal, [Principal]>(100, Principal.equal, Principal.hash);

    // Stable variables for upgrade hooks
    private stable var stableProfiles : [(Principal, UserProfile)] = [];
    private stable var stableUsernames : [(Text, Principal)] = [];
    private stable var stableFollowers : [(Principal, [Principal])] = [];
    private stable var stableFollowing : [(Principal, [Principal])] = [];

    // Upgrade hooks
    system func preupgrade() {
        stableProfiles := Iter.toArray(profiles.entries());
        stableUsernames := Iter.toArray(usernames.entries());
        stableFollowers := Iter.toArray(followers.entries());
        stableFollowing := Iter.toArray(following.entries());
    };

    system func postupgrade() {
        profiles := HashMap.fromIter<Principal, UserProfile>(
            stableProfiles.vals(),
            100,
            Principal.equal,
            Principal.hash,
        );
        usernames := HashMap.fromIter<Text, Principal>(
            stableUsernames.vals(),
            100,
            Text.equal,
            Text.hash,
        );
        followers := HashMap.fromIter<Principal, [Principal]>(
            stableFollowers.vals(),
            100,
            Principal.equal,
            Principal.hash,
        );
        following := HashMap.fromIter<Principal, [Principal]>(
            stableFollowing.vals(),
            100,
            Principal.equal,
            Principal.hash,
        );
        stableProfiles := [];
        stableUsernames := [];
        stableFollowers := [];
        stableFollowing := [];
    };

    // User management functions
    public shared ({ caller }) func createProfile(
        name : Text,
        bio : Text,
        avatar : Text,
    ) : async Result.Result<(), Text> {
        if (Text.size(name) < 3 or Text.size(name) > 20) {
            return #err("Username must be 3-20 characters");
        };

        switch (profiles.get(caller), usernames.get(name)) {
            case (?_, _) { #err("Profile already exists for this principal") };
            case (_, ?_) { #err("Username already taken") };
            case (null, null) {
                let profile : UserProfile = {
                    principal = caller;
                    name = name;
                    bio = bio;
                    avatar = avatar;
                    isCreator = false;
                    totalEarnings = 0;
                    createdAt = Time.now();
                    followersCount = 0;
                    followingCount = 0;
                };
                profiles.put(caller, profile);
                usernames.put(name, caller);
                #ok();
            };
        };
    };

    // Query functions
    public query func getProfile(user : Principal) : async ?UserProfile {
        profiles.get(user);
    };

    public query func getProfileByUsername(username : Text) : async ?UserProfile {
        switch (usernames.get(username)) {
            case (?principal) { profiles.get(principal) };
            case null { null };
        };
    };

    public query func searchProfiles(term : Text) : async [UserProfile] {
        let termLower = Text.map(
            term,
            func(c : Char) : Char {
                Char.fromNat32(Char.toNat32(c) | 32);
            },
        );

        Array.filter<UserProfile>(
            Iter.toArray(profiles.vals()),
            func(profile : UserProfile) {
                let nameLower = Text.map(
                    profile.name,
                    func(c : Char) : Char {
                        Char.fromNat32(Char.toNat32(c) | 32);
                    },
                );
                let bioLower = Text.map(
                    profile.bio,
                    func(c : Char) : Char {
                        Char.fromNat32(Char.toNat32(c) | 32);
                    },
                );
                Text.contains(nameLower, #text termLower) or Text.contains(bioLower, #text termLower);
            }
        );
    };

    // Follow system
    public shared ({ caller }) func follow(user : Principal) : async Result.Result<(), Text> {
        if (caller == user) return #err("Cannot follow yourself");

        switch (profiles.get(user)) {
            case null { #err("User not found") };
            case (?_) {
                // Update follower list
                let currentFollowers = Option.get(followers.get(user), []);
                let alreadyFollowing = Array.find<Principal>(currentFollowers, func(p : Principal) { p == caller }) != null;
                if (not alreadyFollowing) {
                    followers.put(user, Array.append(currentFollowers, [caller]));

                    // Update following list
                    let currentFollowing = Option.get(following.get(caller), []);
                    following.put(caller, Array.append(currentFollowing, [user]));

                    // Update counters
                    updateProfile(user, func(p) { { p with followersCount = p.followersCount + 1 } });
                    updateProfile(caller, func(p) { { p with followingCount = p.followingCount + 1 } });
                };
                #ok();
            };
        };
    };

    // Helper functions
    private func updateProfile(
        user : Principal,
        update : (UserProfile) -> UserProfile,
    ) : () {
        switch (profiles.get(user)) {
            case (?profile) { profiles.put(user, update(profile)) };
            case null {};
        };
    };
};

// frontend/postcss.config.js
module.exports = {
    plugins : {
        tailwindcss : {};
        autoprefixer : {};
    };
};

// frontend/tailwind.config.js
module.exports = {
    content : [
        "./src/**/*.{js,jsx,ts,tsx}",
    ];
    theme : {
        extend : {
            colors : {
                'marketplace' : {
                    100 : '#E6F6FF';
                    200 : '#bae6fd';
                    300 : '#7dd3fc';
                    400 : '#38bdf8';
                    500 : '#0ea5e9';
                    600 : '#0284c7';
                    700 : '#0369a1';
                    800 : '#075985';
                    900 : '#1a365d';
                };
            };
        };
    };
    plugins : [
        require('@tailwindcss/forms'),
    ];
};
