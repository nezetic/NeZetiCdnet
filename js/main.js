(function () {
    "use strict";

    function setupGallery() {
        // Any image whose alt starts with "img_" that is wrapped in a link
        // becomes part of a lightbox gallery.
        document.querySelectorAll('a > img[alt^="img_"]').forEach(function (img) {
            img.parentElement.setAttribute("data-lightbox", "gallery");
        });

        if (window.GLightbox) {
            window.GLightbox({ selector: "[data-lightbox]" });
        }
    }

    function setupThemeToggle() {
        var button = document.getElementById("theme-toggle");
        if (!button) {
            return;
        }

        function render(theme) {
            document.documentElement.setAttribute("data-theme", theme);
        }

        render(document.documentElement.getAttribute("data-theme") || "light");

        button.addEventListener("click", function () {
            var theme = document.documentElement.getAttribute("data-theme") === "dark"
                ? "light"
                : "dark";
            render(theme);
            try {
                localStorage.setItem("theme", theme);
            } catch (err) {
                // Private browsing or storage disabled: the toggle still
                // works, the choice just is not remembered.
            }
        });
    }

    // A plain left click on the language switcher is a manual choice:
    // remember it (normalized to its primary subtag), it wins over any
    // automatic detection.  The redirect itself lives in js/lang.js, loaded
    // from <head> before first paint; this only records the preference.
    // Modified clicks and middle clicks (open in background tab, etc.) do
    // not count.
    function setupLanguageChoice() {
        function primary(tag) {
            return String(tag || "").toLowerCase().split("-")[0];
        }

        document.querySelectorAll("#languages a[hreflang]").forEach(function (link) {
            var lang = primary(link.getAttribute("hreflang"));
            if (!lang) {
                return;
            }
            link.addEventListener("click", function (event) {
                if (event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) {
                    return;
                }
                try {
                    localStorage.setItem("lang", lang);
                } catch (err) {
                    // Storage unavailable: the switch still works.
                }
            });
        });
    }

    function setup() {
        setupGallery();
        setupThemeToggle();
        setupLanguageChoice();
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", setup);
    } else {
        setup();
    }
})();
