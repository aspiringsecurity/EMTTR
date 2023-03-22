/*
 * Copyright 2021 ICON Foundation
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

package foundation.icon.score.example;

import foundation.icon.score.client.ScoreInterface;
import score.Address;
import score.annotation.EventLog;
import score.annotation.External;
import score.annotation.Payable;

import java.math.BigInteger;

@ScoreInterface
public interface IcxTransfer {
    @Payable
    @External
    void transfer(Address _address);

    @External(readonly = true)
    BigInteger getTransferred(Address _address);

    @EventLog(indexed = 1)
    void Transferred(Address _from, BigInteger icx);
}
