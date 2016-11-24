try {
    ga = ga || function() {
        (ga.q = ga.q || []).push(arguments);
    };
    ga.l = +new Date;

    ga("create", "UA-82769478-1", "auto");
    ga("send", "pageview");
} catch (e) {
    console.error(e);
}

try {
    const hljs = require("highlight.js");
    hljs.initHighlighting();
} catch (e) {
    console.error(e);
}
