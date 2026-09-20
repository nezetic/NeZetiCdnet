// Language redirect, loaded synchronously from <head> so the visitor is
// sent to their preferred language before the page paints.
//
// The preferred language is, in order:
//   1. a manual choice (click on the language switcher, stored by main.js);
//   2. the browser language: English by default, only French browsers stay
//      on the French site.
//
// Auto-detection only runs on the home page (marked by <html data-auto-lang>),
// so a deep link is never bounced by it.  A stored manual preference is
// separate and may still bounce any page, at most once per session.  The
// session flag is written only when a redirect actually fires, and the
// redirect is skipped entirely when the requested page has no translation
// (empty data-alt-url): the target is never the other language's home page.

(function () {
    "use strict";

    // Reduce a BCP-47 tag to its primary subtag: "en-GB" -> "en", "fry"
    // stays "fry" (so it never matches "fr").
    function primary(tag) {
        return String(tag || "").toLowerCase().split("-")[0];
    }

    var root = document.documentElement;
    var current = primary(root.getAttribute("lang"));
    var alt = primary(root.getAttribute("data-alt-lang"));
    var altUrl = root.getAttribute("data-alt-url");
    if (!current || !alt || !altUrl || current === alt) {
        return;
    }

    var stored, alreadyRedirected;
    try {
        stored = localStorage.getItem("lang");
        alreadyRedirected = sessionStorage.getItem("lang-redirected");
    } catch (err) {
        // Storage unavailable: never redirect, to avoid loops.
        return;
    }

    stored = primary(stored);

    // Forget a value we no longer recognise (the site's languages changed,
    // or it was written by an older version of this script).  Keeping it
    // would silently disable detection forever.
    if (stored && stored !== current && stored !== alt) {
        try {
            localStorage.removeItem("lang");
        } catch (err) {
            // Ignore: behave as if there were no stored choice.
        }
        stored = "";
    }

    // Once we have bounced this session, stop: manual navigation, deep
    // links and the back button all take precedence from now on.
    if (alreadyRedirected) {
        return;
    }

    // Which language does the visitor want?
    //
    // - A manual choice (click on the switcher) is a persistent preference
    //   enforced at least once per session.
    // - Otherwise, on the home page only, auto-detect from the browser
    //   language: "fr" stays on the French site, everything else is sent to
    //   the English version.  Deep links are never auto-bounced.
    var desired = null;
    if (stored === current || stored === alt) {
        desired = stored;
    } else if (!stored && root.getAttribute("data-auto-lang") === "1") {
        // No manual choice: take the first entry of navigator.languages
        // whose primary subtag matches a language of the site (the current
        // one or the alternate), in the visitor's own preference order.  If
        // none matches, the documented fallback is English: only a French
        // browser stays on the French site, everything else is sent to the
        // English version.  (The site currently has exactly these two
        // languages; the fallback is the only hardcoded tag left.)
        var candidates = (navigator.languages && navigator.languages.length)
            ? navigator.languages
            : [navigator.language];
        for (var i = 0; i < candidates.length && !desired; i++) {
            var sub = primary(candidates[i]);
            if (sub === current || sub === alt) {
                desired = sub;
            }
        }
        if (!desired) {
            desired = "en";
        }
    }

    if (!desired || desired === current) {
        return;
    }

    // Validate the target: it comes from server-rendered markup, but treat
    // it as untrusted anyway.  Only same-origin http(s) URLs are followed.
    var target;
    try {
        target = new URL(altUrl, window.location.href);
    } catch (err) {
        return;
    }
    if ((target.protocol !== "https:" && target.protocol !== "http:")
        || target.origin !== window.location.origin) {
        return;
    }

    try {
        sessionStorage.setItem("lang-redirected", "1");
    } catch (err) {
        // Cannot mark the redirect: stay put rather than risk a loop.
        return;
    }
    window.location.replace(target.href);
})();
