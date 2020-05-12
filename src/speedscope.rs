// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use flamescope::model::*;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::time::Instant;

use crate::cesk;

fn unpack_event<'a>(event: &'a cesk::Event<'a>) -> (&'a str, &'a Instant) {
    match event {
        cesk::Event::Start(label, instant) | cesk::Event::End(label, instant) => (label, instant),
    }
}

pub fn events_to_speedscope_json(events: &Vec<cesk::Event>) -> SpeedscopeFile {
    let labels: HashSet<&str> = events.iter().map(|event| unpack_event(event).0).collect();
    let start = unpack_event(events.first().unwrap()).1;
    let end = unpack_event(events.last().unwrap()).1;
    let labels: Vec<&str> = labels.into_iter().collect();

    let frames = labels
        .iter()
        .map(|&label| Frame::new(Cow::Owned(label.to_owned())))
        .collect();
    let labels: HashMap<&str, usize> = labels
        .into_iter()
        .enumerate()
        .map(|(index, label)| (label, index))
        .collect();
    let events: Vec<Event> = events.iter().map(|event| {
        match event {
            cesk::Event::Start(label, instant) => Event {
                event_type: EventType::OpenFrame,
                at: u64::try_from(instant.duration_since(*start).as_nanos()).unwrap(),
                frame: *labels.get(label).unwrap(),
            },
            cesk::Event::End(label, instant) => Event {
                event_type: EventType::CloseFrame,
                at: u64::try_from(instant.duration_since(*start).as_nanos()).unwrap(),
                frame: *labels.get(label).unwrap(),
            },
        }
    }).collect();
    let profile = Profile::Evented {
        name: Cow::Borrowed("Produced by rusty-engine"),
        unit: ValueUnit::Nanoseconds,
        start_value: 0,
        end_value: u64::try_from(end.duration_since(*start).as_nanos()).unwrap(),
        events,
    };
    SpeedscopeFile {
        schema: "https://www.speedscope.app/file-format-schema.json",
        profiles: vec![profile],
        shared: Shared { frames },
        active_profile_index: Some(0),
        exporter: Some(String::from("rusty-engine")),
        name: Some(String::from("Produced by rusty-engine")),
    }
}
