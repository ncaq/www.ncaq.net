declare var adsbygoogle: any;

interface Window {
  adsbygoogle: any;
}

Array.prototype.forEach.call(
  document.querySelectorAll(".entry-link"),
  (entryLink: HTMLElement, index: number) => {
    if (index !== 0 && index % 15 === 0) {
      const ins = document.createElement("ins");
      ins.setAttribute("class", "adsbygoogle");
      ins.setAttribute("style", "display:block");
      ins.setAttribute("data-ad-format", "fluid");
      ins.setAttribute("data-ad-layout-key", "-cn+57+3d-yw+18s");
      ins.setAttribute("data-ad-client", "ca-pub-8393549016688364");
      ins.setAttribute("data-ad-slot", "4040494271");
      entryLink.appendChild(ins);
      (adsbygoogle = window.adsbygoogle || []).push({});
    }
  }
);
