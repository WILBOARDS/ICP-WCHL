import Principal "mo:base/Principal";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Result "mo:base/Result";
import Option "mo:base/Option";
import Blob "mo:base/Blob";
import Int "mo:base/Int";
import Hash "mo:base/Hash";

actor StorageCanister {
    // Fix Int unbound error by using the imported Int module
    public type Timestamp = Int;

    // Add custom hash function
    private let _hashText = Text.hash;

    // Types
    public type FileId = Text;
    public type FileInfo = {
        id : FileId;
        name : Text;
        contentType : Text;
        size : Nat;
        owner : Principal;
        createdAt : Int;
        isPublic : Bool;
        metadata : [(Text, Text)];
    };

    public type FileChunk = {
        fileId : FileId;
        chunkIndex : Nat;
        data : Blob;
        totalChunks : Nat;
    };

    public type UploadRequest = {
        name : Text;
        contentType : Text;
        size : Nat;
        isPublic : Bool;
        metadata : [(Text, Text)];
    };

    // State
    private var files = HashMap.HashMap<FileId, FileInfo>(100, Text.equal, Text.hash);
    private var fileChunks = HashMap.HashMap<Text, Blob>(1000, Text.equal, Text.hash); // key: fileId-chunkIndex
    private var userFiles = HashMap.HashMap<Principal, [FileId]>(100, Principal.equal, Principal.hash);
    private var userStorage = HashMap.HashMap<Principal, Nat>(100, Principal.equal, Principal.hash);

    // Storage limits
    private let maxFileSize : Nat = 10_000_000; // 10MB
    private let maxChunkSize : Nat = 1_000_000; // 1MB

    // Stable storage
    private stable var stableFiles : [(FileId, FileInfo)] = [];
    private stable var stableFileChunks : [(Text, Blob)] = [];
    private stable var stableUserFiles : [(Principal, [FileId])] = [];
    private stable var stableUserStorage : [(Principal, Nat)] = [];

    system func preupgrade() {
        stableFiles := Iter.toArray(files.entries());
        stableFileChunks := Iter.toArray(fileChunks.entries());
        stableUserFiles := Iter.toArray(userFiles.entries());
        stableUserStorage := Iter.toArray(userStorage.entries());
    };

    system func postupgrade() {
        files := HashMap.fromIter<FileId, FileInfo>(stableFiles.vals(), stableFiles.size(), Text.equal, Text.hash);
        fileChunks := HashMap.fromIter<Text, Blob>(stableFileChunks.vals(), stableFileChunks.size(), Text.equal, Text.hash);
        userFiles := HashMap.fromIter<Principal, [FileId]>(stableUserFiles.vals(), stableUserFiles.size(), Principal.equal, Principal.hash);
        userStorage := HashMap.fromIter<Principal, Nat>(
            stableUserStorage.vals(),
            stableUserStorage.size(),
            Principal.equal,
            Principal.hash,
        );
        stableFiles := [];
        stableFileChunks := [];
        stableUserFiles := [];
        stableUserStorage := [];
    };

    // Helper functions
    private func generateFileId(name : Text, owner : Principal) : FileId {
        let timestamp = Int.toText(Time.now());
        let ownerText = Principal.toText(owner);
        name # "-" # ownerText # "-" # timestamp;
    };

    private func addFileToUser(user : Principal, fileId : FileId) {
        let currentFiles = switch (userFiles.get(user)) {
            case null { [] };
            case (?files) { files };
        };
        userFiles.put(user, Array.append(currentFiles, [fileId]));
    };

    // Public functions
    public func createFile(caller : Principal, request : UploadRequest) : async Result.Result<FileId, Text> {
        if (request.size > maxFileSize) {
            return #err("File size exceeds maximum limit");
        };

        let fileId = generateFileId(request.name, caller);

        let fileInfo : FileInfo = {
            id = fileId;
            name = request.name;
            contentType = request.contentType;
            size = request.size;
            owner = caller;
            createdAt = Time.now();
            isPublic = request.isPublic;
            metadata = request.metadata;
        };

        files.put(fileId, fileInfo);
        addFileToUser(caller, fileId);

        #ok(fileId);
    };

    public func uploadChunk(caller : Principal, chunk : FileChunk) : async Result.Result<(), Text> {
        // Verify file exists and caller owns it
        switch (files.get(chunk.fileId)) {
            case null { return #err("File does not exist") };
            case (?fileInfo) {
                if (fileInfo.owner != caller) {
                    return #err("Not authorized to upload to this file");
                };
            };
        };

        if (chunk.data.size() > maxChunkSize) {
            return #err("Chunk size exceeds maximum limit");
        };

        let chunkKey = chunk.fileId # "-" # Nat.toText(chunk.chunkIndex);
        fileChunks.put(chunkKey, chunk.data);

        #ok();
    };

    public func getFile(caller : ?Principal, fileId : FileId) : async Result.Result<Blob, Text> {
        switch (files.get(fileId)) {
            case null { return #err("File does not exist") };
            case (?fileInfo) {
                // Check access permissions
                if (not fileInfo.isPublic) {
                    switch (caller) {
                        case null {
                            return #err("Authentication required for private file");
                        };
                        case (?user) {
                            if (user != fileInfo.owner) {
                                return #err("Not authorized to access this file");
                            };
                        };
                    };
                };

                // Reconstruct file from chunks
                let totalChunks = Nat.max(1, (fileInfo.size + maxChunkSize - 1) / maxChunkSize);
                var fileData : [Blob] = [];

                for (i in Iter.range(0, totalChunks - 1)) {
                    let chunkKey = fileId # "-" # Nat.toText(i);
                    switch (fileChunks.get(chunkKey)) {
                        case null {
                            return #err("Missing file chunk: " # Nat.toText(i));
                        };
                        case (?chunkData) {
                            fileData := Array.append(fileData, [chunkData]);
                        };
                    };
                };

                // Combine all chunks
                var combinedData = Blob.fromArray([]);
                for (chunk in fileData.vals()) {
                    let currentArray = Blob.toArray(combinedData);
                    let chunkArray = Blob.toArray(chunk);
                    combinedData := Blob.fromArray(Array.append(currentArray, chunkArray));
                };

                #ok(combinedData);
            };
        };
    };

    public func deleteFile(caller : Principal, fileId : FileId) : async Result.Result<(), Text> {
        switch (files.get(fileId)) {
            case null { return #err("File does not exist") };
            case (?fileInfo) {
                if (fileInfo.owner != caller) {
                    return #err("Not authorized to delete this file");
                };

                // Delete file info
                files.delete(fileId);

                // Delete all chunks
                let totalChunks = (fileInfo.size + maxChunkSize - 1) / maxChunkSize;
                for (i in Iter.range(0, totalChunks - 1)) {
                    let chunkKey = fileId # "-" # Nat.toText(i);
                    fileChunks.delete(chunkKey);
                };

                // Remove from user files
                let currentFiles = switch (userFiles.get(caller)) {
                    case null { [] };
                    case (?files) { files };
                };
                let filteredFiles = Array.filter<FileId>(currentFiles, func(id) { id != fileId });
                userFiles.put(caller, filteredFiles);

                #ok();
            };
        };
    };

    // Query functions
    public query func getFileInfo(fileId : FileId) : async ?FileInfo {
        files.get(fileId);
    };

    public query func getUserFiles(user : Principal) : async [FileInfo] {
        let fileIds = switch (userFiles.get(user)) {
            case null { [] };
            case (?ids) { ids };
        };

        Array.mapFilter<FileId, FileInfo>(
            fileIds,
            func(id) {
                files.get(id);
            },
        );
    };

    public query func getPublicFiles() : async [FileInfo] {
        let allFiles = Iter.toArray(files.vals());
        Array.filter<FileInfo>(allFiles, func(fileInfo) { fileInfo.isPublic });
    };

    public query func getFilesByContentType(contentType : Text) : async [FileInfo] {
        let allFiles = Iter.toArray(files.vals());
        Array.filter<FileInfo>(
            allFiles,
            func(fileInfo) {
                fileInfo.isPublic and fileInfo.contentType == contentType
            },
        );
    };

    public query func getTotalStorageUsed() : async Nat {
        let allFiles = Iter.toArray(files.vals());
        Array.foldLeft<FileInfo, Nat>(
            allFiles,
            0,
            func(acc, fileInfo) {
                acc + fileInfo.size;
            },
        );
    };

    public query func getUserStorageUsed(user : Principal) : async Nat {
        switch (userStorage.get(user)) {
            case (?storage) { storage };
            case null { 0 };
        };
    };
};
