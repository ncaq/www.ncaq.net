declare var adsbygoogle;

interface Window {
  adsbygoogle;
}

let adCounter = 0;
function insertNativeFeedAd(url) {
  adCounter += 1;
  if (10 < adCounter) {
    const entry = document.querySelector(`a[href="${url}"]`);
    if (entry) {
      const ins = document.createElement("ins");
      ins.setAttribute("class", "adsbygoogle");
      ins.setAttribute("style", "display:block");
      ins.setAttribute("data-ad-format", "fluid");
      ins.setAttribute("data-ad-layout-key", "-dd+4u+14-qv+17l");
      ins.setAttribute("data-ad-client", "ca-pub-8393549016688364");
      ins.setAttribute("data-ad-slot", "4040494271");
      entry.appendChild(ins);
      (adsbygoogle = window.adsbygoogle || []).push({});
    }
    adCounter = 0;
  }
}
