const jsdom = require('node-jsdom');
const hljs = require('highlight.js');
jsdom.env('/dev/stdin', [], (e, w) => {
    [].forEach.call(w.document.querySelectorAll('pre code'), hljs.highlightBlock);
    console.log(w.document.documentElement.outerHTML);
});
