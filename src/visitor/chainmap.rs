#![allow(dead_code)]
use std::collections::HashMap;

#[derive(Debug)]
pub struct ChainMap<K, V> {
    maps: Vec<HashMap<K, V>>,
}

impl<K, V> ChainMap<K, V>
where
    K: std::cmp::Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self { maps: Vec::new() }
    }

    pub fn pop_map(&mut self) -> Option<HashMap<K, V>> {
        self.maps.pop()
    }

    pub fn insert_map(&mut self) {
        self.maps.push(HashMap::new());
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.maps.last_mut().unwrap().insert(k, v);
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        for map in self.maps.iter().rev() {
            if let Some(v) = map.get(k) {
                return Some(v);
            }
        }
        None
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        for map in self.maps.iter_mut().rev() {
            if let Some(v) = map.get_mut(k) {
                return Some(v);
            }
        }
        None
    }

    pub fn indent_level(&self) -> usize {
        self.maps.len()
    }
}
