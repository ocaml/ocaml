use chrono::{prelude::*, Duration};
use lazy_static::{__Deref, lazy_static};
use reqwest::blocking::Client;
use serde_json::Value;
use std::fs;
use std::path::Path;
use std::sync::Mutex;

lazy_static! {
    static ref IS_ON_STRIKE: Mutex<Option<bool>> = Mutex::new(None);
}

fn find_next_manif() -> String {
    let client = Client::new();
    let response = client
        .get("https://mobilisations-en-france.cgt.fr/api/infos?limit=15")
        .send()
        .expect("couldn't call home");
    let json: Value =
        serde_json::from_str(&response.text().expect("response text")).expect("json decode");

    json["infos"]
        .as_array()
        .unwrap()
        .iter()
        .flat_map(|j| {
            j["eventDate"]
                .as_array()
                .unwrap()
                .iter()
                .map(|d| DateTime::parse_from_rfc3339(d["dateTimeFrom"].as_str().unwrap()).unwrap())
        })
        .filter(|d| Utc::now() < *d + Duration::days(1))
        .min()
        .unwrap()
        .to_rfc3339()
}

fn check_on_strike() -> bool {
    if std::env::var("MAYBE_STRIKE").is_ok() {
        let cache_path = Path::new("/tmp/.ocaml_on_strike");

        let next_manif_start = if cache_path.exists() {
            fs::read_to_string(cache_path).expect("Failed to read cache file")
        } else {
            let manif = find_next_manif();
            fs::write(cache_path, &manif).expect("Failed to write cache file");
            manif
        };

        let date = DateTime::parse_from_rfc3339(&next_manif_start).expect("Failed to parse date");
        let now = Utc::now();

        if now > date + Duration::days(1) {
            // Too far in the past
            fs::remove_file(cache_path).expect("Failed to remove cache file");
            check_on_strike()
        } else {
            now > date && now < date + Duration::days(1)
        }
    } else {
        true
    }
}

#[no_mangle]
pub fn on_strike() -> bool {
    let mut strike = IS_ON_STRIKE.lock().unwrap();
    match strike.deref() {
        Some(v) => *v,
        None => {
            let r = check_on_strike();
            *strike = Some(r);
            r
        }
    }
}
