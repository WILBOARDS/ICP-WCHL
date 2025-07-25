import Principal "mo:base/Principal";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Result "mo:base/Result";
import Int "mo:base/Int";

actor AdvancedNFTMarketplace {
    // Types
    public type TokenId = Nat;
    public type ListingId = Nat;
    public type AuctionId = Nat;
    public type OfferId = Nat;

    public type Listing = {
        id : ListingId;
        tokenId : TokenId;
        seller : Principal;
        price : Nat;
        createdAt : Int;
        expiresAt : ?Int;
        active : Bool;
        listingType : Text; // "fixed", "auction", "bundle"
        bundleTokens : ?[TokenId];
    };

    public type Auction = {
        id : AuctionId;
        tokenId : TokenId;
        seller : Principal;
        startPrice : Nat;
        reservePrice : ?Nat;
        currentBid : Nat;
        currentBidder : ?Principal;
        startTime : Int;
        endTime : Int;
        active : Bool;
        bids : [Bid];
    };

    public type Bid = {
        bidder : Principal;
        amount : Nat;
        timestamp : Int;
    };

    public type Offer = {
        id : OfferId;
        tokenId : TokenId;
        buyer : Principal;
        amount : Nat;
        expiresAt : Int;
        active : Bool;
        createdAt : Int;
    };

    public type CreateListingRequest = {
        tokenId : TokenId;
        price : Nat;
        expiresAt : ?Int;
        listingType : Text;
        bundleTokens : ?[TokenId];
    };

    public type CreateAuctionRequest = {
        tokenId : TokenId;
        startPrice : Nat;
        reservePrice : ?Nat;
        duration : Int;
    };

    public type CreateOfferRequest = {
        tokenId : TokenId;
        amount : Nat;
        duration : Int;
    };

    public type MarketStats = {
        totalVolume : Nat;
        totalSales : Nat;
        activeListings : Nat;
        activeAuctions : Nat;
        floorPrice : ?Nat;
        averagePrice : ?Nat;
    };

    // State
    private stable var nextListingId : ListingId = 1;
    private stable var nextAuctionId : AuctionId = 1;
    private stable var nextOfferId : OfferId = 1;

    private let hashNat = func(n : Nat) : Nat32 {
        Nat32.fromNat(n)
    };

    private var listings = HashMap.HashMap<ListingId, Listing>(100, Nat.equal, hashNat);
    private var auctions = HashMap.HashMap<AuctionId, Auction>(100, Nat.equal, hashNat);
    private var offers = HashMap.HashMap<OfferId, Offer>(100, Nat.equal, hashNat);
    private var activeListings = HashMap.HashMap<TokenId, ListingId>(100, Nat.equal, hashNat);
    private var userListings = HashMap.HashMap<Principal, [ListingId]>(100, Principal.equal, Principal.hash);
    private var userOffers = HashMap.HashMap<Principal, [OfferId]>(100, Principal.equal, Principal.hash);

    // Market statistics
    private stable var totalVolume : Nat = 0;
    private stable var totalSales : Nat = 0;

    // Platform fee (2.5%)
    private let _platformFeePercent : Nat = 250;
    private let _basisPoints : Nat = 10000;

    // NFT canister reference
    private var nftCanister : ?Principal = null;

    // Stable storage
    private stable var stableListings : [(ListingId, Listing)] = [];
    private stable var stableAuctions : [(AuctionId, Auction)] = [];
    private stable var stableOffers : [(OfferId, Offer)] = [];
    private stable var stableActiveListings : [(TokenId, ListingId)] = [];
    private stable var stableUserListings : [(Principal, [ListingId])] = [];
    private stable var stableUserOffers : [(Principal, [OfferId])] = [];

    system func preupgrade() {
        stableListings := Iter.toArray(listings.entries());
        stableAuctions := Iter.toArray(auctions.entries());
        stableOffers := Iter.toArray(offers.entries());
        stableActiveListings := Iter.toArray(activeListings.entries());
        stableUserListings := Iter.toArray(userListings.entries());
        stableUserOffers := Iter.toArray(userOffers.entries());
    };

    system func postupgrade() {
        listings := HashMap.fromIter<ListingId, Listing>(
            stableListings.vals(),
            stableListings.size(),
            Nat.equal,
            hashNat,
        );
        auctions := HashMap.fromIter<AuctionId, Auction>(
            stableAuctions.vals(),
            stableAuctions.size(),
            Nat.equal,
            hashNat,
        );
        offers := HashMap.fromIter<OfferId, Offer>(
            stableOffers.vals(),
            stableOffers.size(),
            Nat.equal,
            hashNat,
        );
        activeListings := HashMap.fromIter<TokenId, ListingId>(
            stableActiveListings.vals(),
            stableActiveListings.size(),
            Nat.equal,
            hashNat,
        );
        userListings := HashMap.fromIter<Principal, [ListingId]>(stableUserListings.vals(), stableUserListings.size(), Principal.equal, Principal.hash);
        userOffers := HashMap.fromIter<Principal, [OfferId]>(stableUserOffers.vals(), stableUserOffers.size(), Principal.equal, Principal.hash);
        stableListings := [];
        stableAuctions := [];
        stableOffers := [];
        stableActiveListings := [];
        stableUserListings := [];
        stableUserOffers := [];
    };

    // Admin function to set NFT canister
    public func setNFTCanister(canisterId : Principal) : async () {
        nftCanister := ?canisterId;
    };

    // Helper functions
    private func addListingToUser(user : Principal, listingId : ListingId) {
        let currentListings = switch (userListings.get(user)) {
            case null { [] };
            case (?listings) { listings };
        };
        userListings.put(user, Array.append(currentListings, [listingId]));
    };

    private func addOfferToUser(user : Principal, offerId : OfferId) {
        let currentOffers = switch (userOffers.get(user)) {
            case null { [] };
            case (?offers) { offers };
        };
        userOffers.put(user, Array.append(currentOffers, [offerId]));
    };

    // Marketplace functions
    public shared ({ caller }) func createListing(request : CreateListingRequest) : async Result.Result<ListingId, Text> {
        // Check if token is already listed
        switch (activeListings.get(request.tokenId)) {
            case (?_) { return #err("Token is already listed") };
            case null {};
        };

        let listingId = nextListingId;
        nextListingId += 1;

        let listing : Listing = {
            id = listingId;
            tokenId = request.tokenId;
            seller = caller;
            price = request.price;
            createdAt = Time.now();
            expiresAt = request.expiresAt;
            active = true;
            listingType = request.listingType;
            bundleTokens = request.bundleTokens;
        };

        listings.put(listingId, listing);
        activeListings.put(request.tokenId, listingId);
        addListingToUser(caller, listingId);

        #ok(listingId);
    };

    public func createAuction(caller : Principal, request : CreateAuctionRequest) : async Result.Result<AuctionId, Text> {
        let auctionId = nextAuctionId;
        nextAuctionId += 1;

        let now = Time.now();
        let auction : Auction = {
            id = auctionId;
            tokenId = request.tokenId;
            seller = caller;
            startPrice = request.startPrice;
            reservePrice = request.reservePrice;
            currentBid = request.startPrice;
            currentBidder = null;
            startTime = now;
            endTime = now + request.duration;
            active = true;
            bids = [];
        };

        auctions.put(auctionId, auction);
        #ok(auctionId);
    };

    public func placeBid(caller : Principal, auctionId : AuctionId, amount : Nat) : async Result.Result<(), Text> {
        let auction = switch (auctions.get(auctionId)) {
            case null { return #err("Auction does not exist") };
            case (?a) { a };
        };

        if (not auction.active) {
            return #err("Auction is not active");
        };

        if (Time.now() > auction.endTime) {
            return #err("Auction has ended");
        };

        if (amount <= auction.currentBid) {
            return #err("Bid must be higher than current bid");
        };

        if (caller == auction.seller) {
            return #err("Cannot bid on your own auction");
        };

        let newBid : Bid = {
            bidder = caller;
            amount = amount;
            timestamp = Time.now();
        };

        let updatedAuction : Auction = {
            id = auction.id;
            tokenId = auction.tokenId;
            seller = auction.seller;
            startPrice = auction.startPrice;
            reservePrice = auction.reservePrice;
            currentBid = amount;
            currentBidder = ?caller;
            startTime = auction.startTime;
            endTime = auction.endTime;
            active = auction.active;
            bids = Array.append(auction.bids, [newBid]);
        };

        auctions.put(auctionId, updatedAuction);
        #ok();
    };

    public func makeOffer(caller : Principal, request : CreateOfferRequest) : async Result.Result<OfferId, Text> {
        let offerId = nextOfferId;
        nextOfferId += 1;

        let offer : Offer = {
            id = offerId;
            tokenId = request.tokenId;
            buyer = caller;
            amount = request.amount;
            expiresAt = Time.now() + request.duration;
            active = true;
            createdAt = Time.now();
        };

        offers.put(offerId, offer);
        addOfferToUser(caller, offerId);
        #ok(offerId);
    };

    public func acceptOffer(_caller : Principal, offerId : OfferId) : async Result.Result<(), Text> {
        let offer = switch (offers.get(offerId)) {
            case null { return #err("Offer does not exist") };
            case (?o) { o };
        };

        if (not offer.active) {
            return #err("Offer is not active");
        };

        if (Time.now() > offer.expiresAt) {
            return #err("Offer has expired");
        };

        // Update offer status
        let updatedOffer : Offer = {
            id = offer.id;
            tokenId = offer.tokenId;
            buyer = offer.buyer;
            amount = offer.amount;
            expiresAt = offer.expiresAt;
            active = false;
            createdAt = offer.createdAt;
        };

        offers.put(offerId, updatedOffer);
        totalVolume += offer.amount;
        totalSales += 1;

        #ok();
    };

    public func buyNFT(caller : Principal, listingId : ListingId, payment : Nat) : async Result.Result<(), Text> {
        let listing = switch (listings.get(listingId)) {
            case null { return #err("Listing does not exist") };
            case (?l) { l };
        };

        if (not listing.active) {
            return #err("Listing is not active");
        };

        if (payment < listing.price) {
            return #err("Insufficient payment");
        };

        if (caller == listing.seller) {
            return #err("Cannot buy your own NFT");
        };

        // Check expiration
        switch (listing.expiresAt) {
            case (?expiry) {
                if (Time.now() > expiry) {
                    return #err("Listing has expired");
                };
            };
            case null {};
        };

        // Update listing status
        let updatedListing : Listing = {
            id = listing.id;
            tokenId = listing.tokenId;
            seller = listing.seller;
            price = listing.price;
            createdAt = listing.createdAt;
            expiresAt = listing.expiresAt;
            active = false;
            listingType = listing.listingType;
            bundleTokens = listing.bundleTokens;
        };

        listings.put(listingId, updatedListing);
        activeListings.delete(listing.tokenId);

        totalVolume += listing.price;
        totalSales += 1;

        #ok();
    };

    // Query functions
    public query func getListing(listingId : ListingId) : async ?Listing {
        listings.get(listingId);
    };

    public query func getAuction(auctionId : AuctionId) : async ?Auction {
        auctions.get(auctionId);
    };

    public query func getOffer(offerId : OfferId) : async ?Offer {
        offers.get(offerId);
    };

    public func getActiveListings() : async [Listing] {
        let allListings = Iter.toArray(listings.vals());
        Array.filter<Listing>(
            allListings,
            func(listing) {
                listing.active and (
                    switch (listing.expiresAt) {
                        case (?expiry) { Time.now() <= expiry };
                        case null { true };
                    }
                )
            },
        );
    };

    public query func getActiveAuctions() : async [Auction] {
        let allAuctions = Iter.toArray(auctions.vals());
        Array.filter<Auction>(
            allAuctions,
            func(auction) {
                auction.active and Time.now() <= auction.endTime
            },
        );
    };

    public query func getTokenOffers(tokenId : TokenId) : async [Offer] {
        let allOffers = Iter.toArray(offers.vals());
        Array.filter<Offer>(
            allOffers,
            func(offer) {
                offer.tokenId == tokenId and offer.active and Time.now() <= offer.expiresAt
            },
        );
    };

    public query func getUserListings(user : Principal) : async [Listing] {
        let listingIds = switch (userListings.get(user)) {
            case null { [] };
            case (?ids) { ids };
        };

        Array.mapFilter<ListingId, Listing>(
            listingIds,
            func(id) {
                listings.get(id);
            },
        );
    };

    public query func getUserOffers(user : Principal) : async [Offer] {
        let offerIds = switch (userOffers.get(user)) {
            case null { [] };
            case (?ids) { ids };
        };

        Array.mapFilter<OfferId, Offer>(
            offerIds,
            func(id) {
                offers.get(id);
            },
        );
    };

    public shared query func getMarketStats() : async MarketStats {
        let activeListingsArray = Iter.toArray(listings.vals());
        let activeListings = Array.filter<Listing>(
            activeListingsArray,
            func(listing) {
                listing.active and (
                    switch (listing.expiresAt) {
                        case (?expiry) { Time.now() <= expiry };
                        case null { true };
                    }
                )
            },
        );

        let activeAuctionsArray = Iter.toArray(auctions.vals());
        let activeAuctions = Array.filter<Auction>(
            activeAuctionsArray,
            func(auction) {
                auction.active and Time.now() <= auction.endTime
            },
        );

        let activeListingsCount = activeListings.size();
        let activeAuctionsCount = activeAuctions.size();

        // Calculate floor price
        let floorPrice = if (activeListings.size() > 0) {
            let prices = Array.map<Listing, Nat>(activeListings, func(listing) { listing.price });
            let sortedPrices = Array.sort<Nat>(prices, Nat.compare);
            ?sortedPrices[0];
        } else {
            null;
        };

        // Calculate average price
        let averagePrice = if (totalSales > 0) {
            ?(totalVolume / totalSales);
        } else {
            null;
        };

        {
            totalVolume;
            totalSales;
            activeListings = activeListingsCount;
            activeAuctions = activeAuctionsCount;
            floorPrice;
            averagePrice;
        };
    };

    public query func getTotalListings() : async Nat {
        listings.size();
    };

    public query func getTotalAuctions() : async Nat {
        auctions.size();
    };
};

{
    "motoko.dfxPath": "dfx";
    "motoko.vessel.enabled": true;
    "editor.formatOnSave": true;
    "[javascript]": {
        "editor.defaultFormatter": "vscode.typescript-language-features"
    },
    "[typescript]": {
        "editor.defaultFormatter": "vscode.typescript-language-features"
    },
    "[motoko]": {
        "editor.defaultFormatter": "dfinity-foundation.vscode-motoko"
    },
    "files.associations": {
        "*.mo": "motoko"
    }
}
