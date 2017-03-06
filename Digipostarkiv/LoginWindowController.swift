//
//  LoginWindowController.swift
//  Digipostarkiv
//
//  Created by Frode on 19/01/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

import Foundation
import Cocoa
import WebKit

class LoginWindowController: NSWindowController {
    
    @IBOutlet weak var loginWindow: NSWindow!
    @IBOutlet weak var loginView: WebView!
    
    func showOauthLoginPage() {
        let oauthUrlString = Sync.oauthUrl("state")
        let oauthUrl = URL(string: oauthUrlString!)
        let request = oauthUrl.map {u in URLRequest(url: u)}
        NSApp.activate(ignoringOtherApps: true)
        loginWindow.makeKeyAndOrderFront(self)
        if let req = request {
            loginView.mainFrame.load(req)
        } else {
            //TODO load error page
        }
    }
    
    func closeOauthLoginPage() {
        loginWindow.close()
        loginView.mainFrameURL = "about:blank"
    }
}
