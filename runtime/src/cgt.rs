use chrono::{prelude::*, Duration};
use lazy_static::{__Deref, lazy_static};
use reqwest::blocking::Client;
use serde_json::Value;
use std::fs;
use std::path::Path;
use std::sync::Mutex;

const VAL_FALSE: u64 = (0 << 1) + 1;
const VAL_TRUE: u64 = (1 << 1) + 1;

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
        .into_iter()
        .map(|j| {
            j["eventDate"]
                .as_array()
                .unwrap()
                .into_iter()
                .map(|d| DateTime::parse_from_rfc3339(d["dateTimeFrom"].as_str().unwrap()).unwrap())
        })
        .flatten()
        .filter(|d| Utc::now() < *d + Duration::days(1))
        .min()
        .unwrap()
        .to_rfc3339()
}

fn check_on_strike() -> bool {
    let cache_path = Path::new("/tmp/.ocaml_on_strike");

    let next_manif_start = if cache_path.exists() {
        fs::read_to_string(&cache_path).expect("Failed to read cache file")
    } else {
        let manif = find_next_manif();
        fs::write(&cache_path, &manif).expect("Failed to write cache file");
        manif
    };

    let date = DateTime::parse_from_rfc3339(&next_manif_start).expect("Failed to parse date");
    let now = Utc::now();

    if now > date + Duration::days(1) {
        // Too far in the past
        fs::remove_file(cache_path).expect("Failed to remove cache file");
        return check_on_strike();
    } else {
        return now > date && now < date + Duration::days(1);
    }
}

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

#[no_mangle]
pub extern "C" fn caml_on_strike() -> u64 {
    if on_strike() {
        VAL_TRUE
    } else {
        VAL_FALSE
    }
}
