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

package foundation.icon.jsonrpc.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import foundation.icon.jsonrpc.Address;
import foundation.icon.jsonrpc.IconStringConverter;

import java.math.BigInteger;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class TransactionResult {
    private Address to;
    private BigInteger cumulativeStepUsed;
    private BigInteger stepUsed;
    private BigInteger stepPrice;
    private List<EventLog> eventLogs;
    private byte[] logsBloom;
    private BigInteger status;
//    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Failure failure;
//    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Address scoreAddress;
    private Hash blockHash;
    private BigInteger blockHeight;
    private BigInteger txIndex;
    private Hash txHash;
//    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Object stepUsedDetails; //[]feePayment

    public Address getTo() {
        return to;
    }

    public BigInteger getCumulativeStepUsed() {
        return cumulativeStepUsed;
    }

    public BigInteger getStepUsed() {
        return stepUsed;
    }

    public BigInteger getStepPrice() {
        return stepPrice;
    }

    public List<EventLog> getEventLogs() {
        return eventLogs;
    }

    public byte[] getLogsBloom() {
        return logsBloom;
    }

    public BigInteger getStatus() {
        return status;
    }

    public Failure getFailure() {
        return failure;
    }

    public Address getScoreAddress() {
        return scoreAddress;
    }

    public Hash getBlockHash() {
        return blockHash;
    }

    public BigInteger getBlockHeight() {
        return blockHeight;
    }

    public BigInteger getTxIndex() {
        return txIndex;
    }

    public Hash getTxHash() {
        return txHash;
    }

    public Object getStepUsedDetails() {
        return stepUsedDetails;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("TransactionResult{");
        sb.append("to=").append(to);
        sb.append(", cumulativeStepUsed=").append(cumulativeStepUsed);
        sb.append(", stepUsed=").append(stepUsed);
        sb.append(", stepPrice=").append(stepPrice);
        sb.append(", eventLogs=").append(eventLogs);
        sb.append(", logsBloom=").append(IconStringConverter.fromBytes(logsBloom));
        sb.append(", status=").append(status);
        sb.append(", failure=").append(failure);
        sb.append(", scoreAddress=").append(scoreAddress);
        sb.append(", blockHash=").append(blockHash);
        sb.append(", blockHeight=").append(blockHeight);
        sb.append(", txIndex=").append(txIndex);
        sb.append(", txHash=").append(txHash);
        sb.append(", stepUsedDetails=").append(stepUsedDetails);
        sb.append('}');
        return sb.toString();
    }

    public static class EventLog {
        private Address scoreAddress;
        private List<String> indexed;
        private List<String> data;

        public Address getScoreAddress() {
            return scoreAddress;
        }

        public List<String> getIndexed() {
            return indexed;
        }

        public List<String> getData() {
            return data;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder("EventLog{");
            sb.append("scoreAddress=").append(scoreAddress);
            sb.append(", indexed=").append(indexed);
            sb.append(", data=").append(data);
            sb.append('}');
            return sb.toString();
        }
    }

    public static class Failure {
        private BigInteger code;
        private String message;

        public BigInteger getCode() {
            return code;
        }

        public String getMessage() {
            return message;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder("Failure{");
            sb.append("code=").append(code);
            sb.append(", message='").append(message).append('\'');
            sb.append('}');
            return sb.toString();
        }
    }
}
