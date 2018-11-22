declare var adsbygoogle: any;

interface Window {
  adsbygoogle: any;
}

let adCounter = 0;
function insertNativeFeedAd(url: string) {
  adCounter += 1;
  if (15 < adCounter) {
    const entry = document.querySelector(`a[href="${url}"]`);
    if (entry) {
      const ins = document.createElement("ins");
      ins.setAttribute("class", "adsbygoogle");
      ins.setAttribute("style", "display:block");
      ins.setAttribute("data-ad-format", "fluid");
      ins.setAttribute("data-ad-layout-key", "-cn+57+3d-yw+18s");
      ins.setAttribute("data-ad-client", "ca-pub-8393549016688364");
      ins.setAttribute("data-ad-slot", "4040494271");
      entry.appendChild(ins);
      (adsbygoogle = window.adsbygoogle || []).push({});
    }
    adCounter = 0;
  }
}
