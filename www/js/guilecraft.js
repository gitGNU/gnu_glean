$(document).ready(glean_toggle($("div.help"), 0));

// Preliminary Help Hide functionality
$("div.navbar li:nth-last-child(1)").on("click", function() {glean_toggle($("div.help"), 'fast');});

function glean_toggle(what, duration) {
    what.animate({height: "toggle"}, duration, function() {});
    // if (what.css("display") == "none") {
    //     what.css("display", "auto");
    // } else {
    //     what.css("display", "none");
    // }
};
