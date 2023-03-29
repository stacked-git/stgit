// SPDX-License-Identifier: GPL-2.0-only

//! Implementations for [`LocationConstraint`], [`LocationGroup`], and [`RangeConstraint`].

use super::{LocationConstraint, LocationGroup, RangeConstraint};

impl std::fmt::Display for LocationGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LocationGroup::Applied => "applied",
            LocationGroup::Unapplied => "unapplied",
            LocationGroup::Hidden => "hidden",
        }
        .fmt(f)
    }
}

impl RangeConstraint {
    pub(super) fn use_applied_boundary(self) -> bool {
        matches!(
            self,
            RangeConstraint::AllWithAppliedBoundary | RangeConstraint::VisibleWithAppliedBoundary
        )
    }
}

impl From<RangeConstraint> for LocationConstraint {
    fn from(value: RangeConstraint) -> Self {
        match value {
            RangeConstraint::All | RangeConstraint::AllWithAppliedBoundary => {
                LocationConstraint::All
            }
            RangeConstraint::Visible | RangeConstraint::VisibleWithAppliedBoundary => {
                LocationConstraint::Visible
            }
            RangeConstraint::Applied => LocationConstraint::Applied,
            RangeConstraint::Unapplied => LocationConstraint::Unapplied,
            RangeConstraint::Hidden => LocationConstraint::Hidden,
        }
    }
}
