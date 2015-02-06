//
//  Digipostarkiv
//
//  Created by Frode on 04/02/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

import Foundation

extension Optional {

    func flatMap<U>(f: (a: T) -> Optional<U>) -> Optional<U> {
        switch (self) {
            case .None: return nil
            case .Some(let value): return f(a: value)
        }
    }
}