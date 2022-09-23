const name = "Loris";
pub const coolness: u8 = 100;
/// My comment
const makes_test_files_by_hand = true;

/// This struct has multiple
/// lines of comments
const MyStruct = struct {
    /// so does this
    /// field!
    rimu_is_here: bool,

    /// and this sub
    /// struct
    const MyStructInAStruct = struct {
        /// and this substruct's
        /// field
        yay: i32,

        fn bruh() void {}
    };
};

/// haha funny
var joe_mama: usize = 3;

pub fn otherFunc() void {}

/// yo
pub fn myFunc() void {
    var testing = 123;
    _ = testing;
    joe_mama = 420;

    otherFunc();
    myFunc();

    {
        var t = 3;
        _ = t;
    }
    MyStruct.MyStructInAStruct.bruh();
}
