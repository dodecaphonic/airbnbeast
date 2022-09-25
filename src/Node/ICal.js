import ical from "node-ical";

export function _parseICS(icsString) {
  return ical.sync.parseICS(icsString.trim());
}

export async function _fetchICS(url) {
  return ical.async.fromURL(url);
}
