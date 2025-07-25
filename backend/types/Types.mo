module {
    public type TokenId = Nat;
    public type ListingId = Nat;
    public type AuctionId = Nat;
    public type OfferId = Nat;

    public type Listing = {
        id: ListingId;
        tokenId: TokenId;
        seller: Principal;
        price: Nat;
        createdAt: Int;
        expiresAt: ?Int;
        active: Bool;
        listingType: Text;
        bundleTokens: ?[TokenId];
    };

    // Add other shared types here...
}