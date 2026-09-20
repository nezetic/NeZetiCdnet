// Sets the theme on <html> before first paint to avoid a flash: the stored
// choice wins, otherwise the system preference. Loaded synchronously from
// <head>, before the stylesheet is applied.
(function () {
    "use strict";

    var theme = localStorage.getItem("theme");
    if (theme !== "dark" && theme !== "light") {
        theme = window.matchMedia("(prefers-color-scheme: dark)").matches
            ? "dark"
            : "light";
    }
    document.documentElement.setAttribute("data-theme", theme);
})();
