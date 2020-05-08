// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
pub trait Unpack {
    type Item;

    // TODO(MH): Once const generics (https://github.com/rust-lang/rust/issues/44580) become stable,
    // replace all methods by the single method:
    // ```
    // fn unpackN<const N: usize>(self) -> [Self::Item; N];
    // ```
    fn unpack1(self) -> Self::Item;
    fn unpack2(self) -> (Self::Item, Self::Item);
    fn unpack3(self) -> (Self::Item, Self::Item, Self::Item);
}

impl<T> Unpack for Vec<T> {
    type Item = T;

    fn unpack1(mut self) -> T {
        assert_eq!(self.len(), 1);
        unsafe {
            self.set_len(0);
            std::ptr::read(self.as_ptr() as *const T)
        }
    }

    fn unpack2(mut self) -> (T, T) {
        assert_eq!(self.len(), 2);
        unsafe {
            self.set_len(0);
            std::ptr::read(self.as_ptr() as *const (T, T))
        }
    }

    fn unpack3(mut self) -> (T, T, T) {
        assert_eq!(self.len(), 3);
        unsafe {
            self.set_len(0);
            std::ptr::read(self.as_ptr() as *const (T, T, T))
        }
    }
}
