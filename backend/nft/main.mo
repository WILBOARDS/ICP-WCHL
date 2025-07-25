import Principal "mo:base/Principal";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Result "mo:base/Result";
import Nat32 "mo:base/Nat32";

actor class NFTToken() {
    private let hashNat = func(n : Nat) : Nat32 {
        Nat32.fromNat(n)
    };

    // Types
    public type TokenId = Nat;
    public type CollectionId = Nat;

    public type TokenMetadata = {
        name : Text;
        description : Text;
        image : Text;
        animationUrl : ?Text;
        externalUrl : ?Text;
        attributes : [(Text, Text)];
        rarity : Text; // Common, Rare, Epic, Legendary
        category : Text; // Art, Music, Gaming, etc.
    };

    public type Collection = {
        id : CollectionId;
        name : Text;
        description : Text;
        image : Text;
        creator : Principal;
        royaltyPercent : Nat; // Basis points (100 = 1%)
        isVerified : Bool;
        createdAt : Int;
        totalSupply : Nat;
        floorPrice : ?Nat;
    };

    public type NFT = {
        id : TokenId;
        owner : Principal;
        metadata : TokenMetadata;
        createdAt : Int;
        creator : Principal;
        collectionId : ?CollectionId;
        royaltyPercent : Nat;
        transferHistory : [TransferRecord];
        likes : Nat;
        views : Nat;
        isLocked : Bool; // For staking/lending
    };

    public type TransferRecord = {
        from : Principal;
        to : Principal;
        price : ?Nat;
        timestamp : Int;
        txType : Text; // "mint", "transfer", "sale"
    };

    public type MintRequest = {
        to : Principal;
        metadata : TokenMetadata;
        collectionId : ?CollectionId;
        royaltyPercent : Nat;
        lazyMint : Bool;  // If true, only creates metadata without minting
    };

    public type BulkMintRequest = {
        to : Principal;
        metadataList : [TokenMetadata];
        collectionId : ?CollectionId;
        royaltyPercent : Nat;
        lazyMint : Bool;
    };

    public type TransferRequest = {
        tokenId : TokenId;
        to : Principal;
        price : ?Nat;
        txType : Text;
    };

    public type CreateCollectionRequest = {
        name : Text;
        description : Text;
        image : Text;
        royaltyPercent : Nat;
    };

    // State
    private stable var nextTokenId : TokenId = 1;
    private stable var nextCollectionId : CollectionId = 1;
    private var tokens = HashMap.HashMap<TokenId, NFT>(100, Nat.equal, hashNat);
    private var collections = HashMap.HashMap<CollectionId, Collection>(50, Nat.equal, hashNat);
    private var owners = HashMap.HashMap<Principal, [TokenId]>(100, Principal.equal, Principal.hash);
    private var pendingMints = HashMap.HashMap<TokenId, MintRequest>(100, Nat.equal, hashNat);
    private var collectionTokens = HashMap.HashMap<CollectionId, [TokenId]>(50, Nat.equal, hashNat);
    private var userLikes = HashMap.HashMap<Principal, [TokenId]>(100, Principal.equal, Principal.hash);
    private var creatorEarnings = HashMap.HashMap<Principal, Nat>(100, Principal.equal, Principal.hash);

    // Stable storage for upgrades
    private stable var stableTokens : [(TokenId, NFT)] = [];
    private stable var stableCollections : [(CollectionId, Collection)] = [];
    private stable var stableOwners : [(Principal, [TokenId])] = [];
    private stable var stableCollectionTokens : [(CollectionId, [TokenId])] = [];
    private stable var stableUserLikes : [(Principal, [TokenId])] = [];
    private stable var stableCreatorEarnings : [(Principal, Nat)] = [];
    private stable var stablePendingMints : [(TokenId, MintRequest)] = [];

    system func preupgrade() {
        stableTokens := Iter.toArray(tokens.entries());
        stableCollections := Iter.toArray(collections.entries());
        stableOwners := Iter.toArray(owners.entries());
        stableCollectionTokens := Iter.toArray(collectionTokens.entries());
        stableUserLikes := Iter.toArray(userLikes.entries());
        stableCreatorEarnings := Iter.toArray(creatorEarnings.entries());
        stablePendingMints := Iter.toArray(pendingMints.entries());
    };

    system func postupgrade() {
        tokens := HashMap.fromIter<TokenId, NFT>(stableTokens.vals(), stableTokens.size(), Nat.equal, hashNat);
        collections := HashMap.fromIter<CollectionId, Collection>(stableCollections.vals(), stableCollections.size(), Nat.equal, hashNat);
        owners := HashMap.fromIter<Principal, [TokenId]>(stableOwners.vals(), stableOwners.size(), Principal.equal, Principal.hash);
        collectionTokens := HashMap.fromIter<CollectionId, [TokenId]>(stableCollectionTokens.vals(), stableCollectionTokens.size(), Nat.equal, hashNat);
        userLikes := HashMap.fromIter<Principal, [TokenId]>(stableUserLikes.vals(), stableUserLikes.size(), Principal.equal, Principal.hash);
        creatorEarnings := HashMap.fromIter<Principal, Nat>(stableCreatorEarnings.vals(), stableCreatorEarnings.size(), Principal.equal, Principal.hash);
        pendingMints := HashMap.fromIter<TokenId, MintRequest>(stablePendingMints.vals(), stablePendingMints.size(), Nat.equal, hashNat);
        stableTokens := [];
        stableCollections := [];
        stableOwners := [];
        stableCollectionTokens := [];
        stableUserLikes := [];
        stableCreatorEarnings := [];
    };

    // Helper functions
    private func addTokenToOwner(owner : Principal, tokenId : TokenId) {
        let currentTokens = switch (owners.get(owner)) {
            case null { [] };
            case (?tokens) { tokens };
        };
        owners.put(owner, Array.append(currentTokens, [tokenId]));
    };

    private func removeTokenFromOwner(owner : Principal, tokenId : TokenId) {
        let currentTokens = switch (owners.get(owner)) {
            case null { [] };
            case (?tokens) { tokens };
        };
        let filteredTokens = Array.filter<TokenId>(currentTokens, func(id) { id != tokenId });
        owners.put(owner, filteredTokens);
    };

    private func addTokenToCollection(collectionId : CollectionId, tokenId : TokenId) {
        let currentTokens = switch (collectionTokens.get(collectionId)) {
            case null { [] };
            case (?tokens) { tokens };
        };
        collectionTokens.put(collectionId, Array.append(currentTokens, [tokenId]));
    };

    private func updateCollectionStats(collectionId : CollectionId) {
        switch (collections.get(collectionId)) {
            case null {};
            case (?collection) {
                let tokenIds = switch (collectionTokens.get(collectionId)) {
                    case null { [] };
                    case (?ids) { ids };
                };

                let updatedCollection : Collection = {
                    id = collection.id;
                    name = collection.name;
                    description = collection.description;
                    image = collection.image;
                    creator = collection.creator;
                    royaltyPercent = collection.royaltyPercent;
                    isVerified = collection.isVerified;
                    createdAt = collection.createdAt;
                    totalSupply = tokenIds.size();
                    floorPrice = collection.floorPrice;
                };
                collections.put(collectionId, updatedCollection);
            };
        };
    };

    // Public functions
    public func createCollection(caller : Principal, request : CreateCollectionRequest) : async Result.Result<CollectionId, Text> {
        let collectionId = nextCollectionId;
        nextCollectionId += 1;

        let collection : Collection = {
            id = collectionId;
            name = request.name;
            description = request.description;
            image = request.image;
            creator = caller;
            royaltyPercent = request.royaltyPercent;
            isVerified = false;
            createdAt = Time.now();
            totalSupply = 0;
            floorPrice = null;
        };

        collections.put(collectionId, collection);
        #ok(collectionId);
    };

    public func bulkMint(caller : Principal, request : BulkMintRequest) : async Result.Result<[TokenId], Text> {
        var tokenIds : [TokenId] = [];
        
        for (metadata in request.metadataList.vals()) {
            let mintReq : MintRequest = {
                to = request.to;
                metadata = metadata;
                collectionId = request.collectionId;
                royaltyPercent = request.royaltyPercent;
                lazyMint = request.lazyMint;
            };
            
            switch (await mint(caller, mintReq)) {
                case (#ok(tokenId)) { 
                    tokenIds := Array.append(tokenIds, [tokenId]);
                };
                case (#err(e)) { 
                    return #err("Failed to mint token: " # e);
                };
            };
        };
        
        #ok(tokenIds);
    };

    public func mint(caller : Principal, request : MintRequest) : async Result.Result<TokenId, Text> {
        let tokenId = nextTokenId;
        nextTokenId += 1;

        let transferRecord : TransferRecord = {
            from = caller;
            to = request.to;
            price = null;
            timestamp = Time.now();
            txType = "mint";
        };

        // If lazy minting, we don't create the actual NFT until purchase
        if (request.lazyMint) {
            pendingMints.put(tokenId, request);
            return #ok(tokenId);
        };

        let nft : NFT = {
            id = tokenId;
            owner = request.to;
            metadata = request.metadata;
            createdAt = Time.now();
            creator = caller;
            collectionId = request.collectionId;
            royaltyPercent = request.royaltyPercent;
            transferHistory = [transferRecord];
            likes = 0;
            views = 0;
            isLocked = false;
        };

        tokens.put(tokenId, nft);
        addTokenToOwner(request.to, tokenId);

        switch (request.collectionId) {
            case null {};
            case (?colId) {
                addTokenToCollection(colId, tokenId);
                updateCollectionStats(colId);
            };
        };

        #ok(tokenId);
    };

    public func transfer(caller : Principal, request : TransferRequest) : async Result.Result<(), Text> {
        switch (tokens.get(request.tokenId)) {
            case null { #err("Token does not exist") };
            case (?nft) {
                if (nft.owner != caller) {
                    return #err("Not the owner of this token");
                };

                if (nft.isLocked) {
                    return #err("Token is locked and cannot be transferred");
                };

                let transferRecord : TransferRecord = {
                    from = caller;
                    to = request.to;
                    price = request.price;
                    timestamp = Time.now();
                    txType = request.txType;
                };

                let updatedNft : NFT = {
                    id = nft.id;
                    owner = request.to;
                    metadata = nft.metadata;
                    createdAt = nft.createdAt;
                    creator = nft.creator;
                    collectionId = nft.collectionId;
                    royaltyPercent = nft.royaltyPercent;
                    transferHistory = Array.append(nft.transferHistory, [transferRecord]);
                    likes = nft.likes;
                    views = nft.views;
                    isLocked = nft.isLocked;
                };

                tokens.put(request.tokenId, updatedNft);
                removeTokenFromOwner(caller, request.tokenId);
                addTokenToOwner(request.to, request.tokenId);

                // Update creator earnings if it's a sale
                switch (request.price) {
                    case null {};
                    case (?price) {
                        let royaltyAmount = (price * nft.royaltyPercent) / 10000;
                        let currentEarnings = switch (creatorEarnings.get(nft.creator)) {
                            case null { 0 };
                            case (?earnings) { earnings };
                        };
                        creatorEarnings.put(nft.creator, currentEarnings + royaltyAmount);
                    };
                };

                #ok();
            };
        };
    };

    public shared func likeToken(tokenId : TokenId) : async Result.Result<(), Text> {
        switch (tokens.get(tokenId)) {
            case null { #err("Token does not exist") };
            case (?nft) {
                let updatedNft = {
                    id = nft.id;
                    owner = nft.owner;
                    metadata = nft.metadata;
                    createdAt = nft.createdAt;
                    creator = nft.creator;
                    collectionId = nft.collectionId;
                    royaltyPercent = nft.royaltyPercent;
                    transferHistory = nft.transferHistory;
                    likes = nft.likes + 1;
                    views = nft.views;
                    isLocked = nft.isLocked;
                };

                tokens.put(tokenId, updatedNft);
                #ok();
            };
        };
    };

    public func lockToken(caller : Principal, tokenId : TokenId) : async Result.Result<(), Text> {
        switch (tokens.get(tokenId)) {
            case null { #err("Token does not exist") };
            case (?nft) {
                if (nft.owner != caller) {
                    return #err("Not the owner of this token");
                };

                let updatedNft : NFT = {
                    id = nft.id;
                    owner = nft.owner;
                    metadata = nft.metadata;
                    createdAt = nft.createdAt;
                    creator = nft.creator;
                    collectionId = nft.collectionId;
                    royaltyPercent = nft.royaltyPercent;
                    transferHistory = nft.transferHistory;
                    likes = nft.likes;
                    views = nft.views;
                    isLocked = true;
                };

                tokens.put(tokenId, updatedNft);
                #ok();
            };
        };
    };

    // Query functions
    public query func getToken(tokenId : TokenId) : async ?NFT {
        tokens.get(tokenId);
    };

    public query func getCollection(collectionId : CollectionId) : async ?Collection {
        collections.get(collectionId);
    };

    public query func getTokensByOwner(owner : Principal) : async [NFT] {
        let tokenIds = switch (owners.get(owner)) {
            case null { [] };
            case (?ids) { ids };
        };

        Array.mapFilter<TokenId, NFT>(
            tokenIds,
            func(id) {
                tokens.get(id);
            },
        );
    };

    public query func getTokensByCollection(collectionId : CollectionId) : async [NFT] {
        let tokenIds = switch (collectionTokens.get(collectionId)) {
            case null { [] };
            case (?ids) { ids };
        };

        Array.mapFilter<TokenId, NFT>(
            tokenIds,
            func(id) {
                tokens.get(id);
            },
        );
    };

    public query func getAllTokens() : async [NFT] {
        Iter.toArray(tokens.vals());
    };

    public query func getAllCollections() : async [Collection] {
        Iter.toArray(collections.vals());
    };

    public query func getTokensByRarity(rarity : Text) : async [NFT] {
        let allTokens = Iter.toArray(tokens.vals());
        Array.filter<NFT>(
            allTokens,
            func(nft) {
                nft.metadata.rarity == rarity;
            },
        );
    };

    public query func getTokensByCategory(category : Text) : async [NFT] {
        let allTokens = Iter.toArray(tokens.vals());
        Array.filter<NFT>(
            allTokens,
            func(nft) {
                nft.metadata.category == category;
            },
        );
    };

    public query func getTrendingTokens() : async [NFT] {
        let allTokens = Iter.toArray(tokens.vals());
        let sortedByViews = Array.sort<NFT>(
            allTokens,
            func(a, b) {
                if (a.views > b.views) { #less } else if (a.views < b.views) {
                    #greater;
                } else { #equal };
            },
        );

        let size = sortedByViews.size();
        if (size > 10) {
            Array.tabulate<NFT>(10, func(i) { sortedByViews[i] });
        } else {
            sortedByViews;
        };
    };

    public query func getUserLikedTokens(user : Principal) : async [NFT] {
        let likedTokenIds = switch (userLikes.get(user)) {
            case null { [] };
            case (?ids) { ids };
        };

        Array.mapFilter<TokenId, NFT>(
            likedTokenIds,
            func(id) {
                tokens.get(id);
            },
        );
    };

    public query func getCreatorEarnings(creator : Principal) : async Nat {
        switch (creatorEarnings.get(creator)) {
            case null { 0 };
            case (?earnings) { earnings };
        };
    };

    public query func getTokenHistory(tokenId : TokenId) : async ?[TransferRecord] {
        switch (tokens.get(tokenId)) {
            case null { null };
            case (?nft) { ?nft.transferHistory };
        };
    };

    public query func ownerOf(tokenId : TokenId) : async ?Principal {
        switch (tokens.get(tokenId)) {
            case null { null };
            case (?nft) { ?nft.owner };
        };
    };

    public query func totalSupply() : async Nat {
        tokens.size();
    };

    public query func exists(tokenId : TokenId) : async Bool {
        switch (tokens.get(tokenId)) {
            case null { false };
            case (?_) { true };
        };
    };

    public query func isTokenLocked(tokenId : TokenId) : async Bool {
        switch (tokens.get(tokenId)) {
            case null { false };
            case (?nft) { nft.isLocked };
        };
    };
};
