/*
 * Copyright 2023 ICON Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package foundation.icon.score.client;

import foundation.icon.icx.KeyWallet;
import foundation.icon.icx.crypto.KeystoreException;
import foundation.icon.jsonrpc.Address;

import java.io.File;
import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;

public class Wallet {
    private final foundation.icon.icx.KeyWallet wallet;

    private final byte[] publicKey;
    private final Address address;

    public Wallet(foundation.icon.icx.KeyWallet wallet) {
        this.wallet = wallet;
        publicKey = wallet.getPublicKey().toByteArray();
        address = new Address(wallet.getAddress().toString());
    }

    public byte[] getPublicKey() {
        return publicKey;
    }

    public Address getAddress() {
        return address;
    }

    public byte[] sign(byte[] data) {
        return wallet.sign(data);
    }

    public KeyWallet getWallet() {
        return wallet;
    }

    public static Wallet load(String password, File file) {
        try {
            return new Wallet(KeyWallet.load(password, file));
        } catch (IOException | KeystoreException e) {
            throw new RuntimeException(e);
        }
    }

    public static Wallet generate() {
        try {
            return new Wallet(KeyWallet.create());
        } catch (InvalidAlgorithmParameterException | NoSuchAlgorithmException | NoSuchProviderException e) {
            throw new RuntimeException(e);
        }
    }

}
