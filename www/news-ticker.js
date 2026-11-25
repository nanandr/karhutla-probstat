(function () {
  "use strict";

  const newsFeedUrl = new URL("https://news.google.com/rss/search");
  newsFeedUrl.searchParams.set("q", "karhutla");
  newsFeedUrl.searchParams.set("hl", "id");
  newsFeedUrl.searchParams.set("gl", "ID");
  newsFeedUrl.searchParams.set("ceid", "ID:id");

  const apiUrl = new URL("https://api.rss2json.com/v1/api.json");
  apiUrl.searchParams.set("rss_url", newsFeedUrl.href);
  const track = document.getElementById("ticker-track");
  const primary = document.getElementById("ticker-primary");
  const copy = document.getElementById("ticker-copy");
  const clock = document.getElementById("ticker-clock");

  if (!track || !primary || !copy) return;

  const fallbackHeadlines = [
    "Pencegahan Karhutla · Hindari membuka lahan dengan cara membakar dan jangan meninggalkan api tanpa pengawasan.",
    "Kesiapsiagaan · Laporkan asap atau titik api kepada petugas setempat sedini mungkin.",
    "Info Karhutla · Pantau peringatan resmi saat cuaca kering dan hindari aktivitas yang dapat memicu api."
  ];

  function updateClock() {
    if (!clock) return;
    const parts = new Intl.DateTimeFormat("id-ID", {
      timeZone: "Asia/Jakarta",
      hour: "2-digit",
      minute: "2-digit",
      hourCycle: "h23"
    }).formatToParts(new Date());
    const hour = parts.find((part) => part.type === "hour")?.value;
    const minute = parts.find((part) => part.type === "minute")?.value;
    if (hour && minute) clock.textContent = `${hour}:${minute} WIB`;
  }

  function createStory(sourceName, content) {
    const story = document.createElement("span");
    story.className = "ticker-story";

    const source = document.createElement("span");
    source.className = "ticker-source";
    source.textContent = sourceName;
    story.append(source, content);
    return story;
  }

  function articleStory(article) {
    if (!article || typeof article.title !== "string" || !article.title.trim()) {
      return null;
    }

    let url;
    try {
      url = new URL(article.link);
    } catch (error) {
      return null;
    }
    if (url.protocol !== "https:" && url.protocol !== "http:") return null;

    const sourceMatch = article.title.match(/\s-\s([^-]+)$/);
    const sourceName =
      (typeof article.author === "string" && article.author.trim()) ||
      (sourceMatch && sourceMatch[1].trim()) ||
      url.hostname;
    const link = document.createElement("a");
    link.href = url.href;
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    link.textContent = article.title.trim();
    link.setAttribute("aria-label", `${link.textContent} — ${sourceName}`);

    return createStory(sourceName, link);
  }

  function fallbackStory(headline) {
    const text = document.createElement("span");
    text.className = "ticker-info-text";
    text.textContent = headline;
    return createStory("INFO DUMMY", text);
  }

  function showStories(stories) {
    primary.replaceChildren(...stories);
    copy.replaceChildren(
      ...stories.map((story) => {
        const duplicate = story.cloneNode(true);
        duplicate.querySelectorAll("a").forEach((link) => {
          link.tabIndex = -1;
        });
        return duplicate;
      })
    );
    track.classList.remove("is-loading");
  }

  function showFallback() {
    showStories(fallbackHeadlines.map(fallbackStory));
  }

  async function loadHeadlines() {
    const controller = new AbortController();
    const timeout = window.setTimeout(() => controller.abort(), 8000);

    try {
      const response = await fetch(apiUrl, {
        headers: { Accept: "application/json" },
        signal: controller.signal
      });
      if (!response.ok) {
        throw new Error(`News API returned HTTP ${response.status}`);
      }

      const result = await response.json();
      const stories = result.status === "ok" && Array.isArray(result.items)
        ? result.items.map(articleStory).filter(Boolean)
        : [];
      if (stories.length > 0) {
        showStories(stories);
      } else {
        showFallback();
      }
    } catch (error) {
      console.warn("Berita karhutla tidak tersedia; menampilkan info dummy.", error);
      showFallback();
    } finally {
      window.clearTimeout(timeout);
    }
  }

  updateClock();
  window.setInterval(updateClock, 15000);
  showFallback();
  loadHeadlines();
  window.setInterval(loadHeadlines, 15 * 60 * 1000);
})();
