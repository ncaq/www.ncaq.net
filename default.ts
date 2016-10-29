interface Window {
    ga: any;
}

try {
    window.ga = window.ga || function() {
        (window.ga.q = window.ga.q || []).push(arguments);
    };
    window.ga.l = +new Date;

    window.ga("create", "UA-82769478-1", "auto");
    window.ga("send", "pageview");
} catch (e) {
    console.error(e);
}

declare namespace hljs {
    export function initHighlighting(): void;
}

try {
    const hljsWaitLoad = () => {
        try {
            const hljs = require("highlight.js");
            hljs.initHighlighting();
        } catch (e) {
            setTimeout(hljsWaitLoad, 100);
        }
    };
    hljsWaitLoad();
} catch (e) {
    console.error(e);
}
