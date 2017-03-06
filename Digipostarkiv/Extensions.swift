//
//  Digipostarkiv
//
//  Created by Frode on 04/02/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

import Foundation

extension Optional {

    func flatMap<U>(_ f: (_ a: Wrapped) -> Optional<U>) -> Optional<U> {
        switch (self) {
            case .none: return nil
            case .some(let value): return f(value)
        }
    }
}
