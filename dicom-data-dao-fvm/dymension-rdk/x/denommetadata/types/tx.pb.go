// Code generated by protoc-gen-gogo. DO NOT EDIT.
// source: denommetadata/tx.proto

package types

import (
	context "context"
	fmt "fmt"
	_ "github.com/gogo/protobuf/gogoproto"
	grpc1 "github.com/gogo/protobuf/grpc"
	proto "github.com/gogo/protobuf/proto"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
	io "io"
	math "math"
	math_bits "math/bits"
)

// Reference imports to suppress errors if they are not otherwise used.
var _ = proto.Marshal
var _ = fmt.Errorf
var _ = math.Inf

// This is a compile-time assertion to ensure that this generated file
// is compatible with the proto package it is being compiled against.
// A compilation error at this line likely means your copy of the
// proto package needs to be updated.
const _ = proto.GoGoProtoPackageIsVersion3 // please upgrade the proto package

// MsgCreateDenomMetadata defines a message that create the denom metadata.
type MsgCreateDenomMetadata struct {
	// sender_address is the bech32 address of message sender.
	SenderAddress string          `protobuf:"bytes,1,opt,name=sender_address,json=senderAddress,proto3" json:"sender_address,omitempty"`
	Metadatas     []DenomMetadata `protobuf:"bytes,2,rep,name=metadatas,proto3" json:"metadatas"`
}

func (m *MsgCreateDenomMetadata) Reset()         { *m = MsgCreateDenomMetadata{} }
func (m *MsgCreateDenomMetadata) String() string { return proto.CompactTextString(m) }
func (*MsgCreateDenomMetadata) ProtoMessage()    {}
func (*MsgCreateDenomMetadata) Descriptor() ([]byte, []int) {
	return fileDescriptor_a5bb4e1c9698e3b7, []int{0}
}
func (m *MsgCreateDenomMetadata) XXX_Unmarshal(b []byte) error {
	return m.Unmarshal(b)
}
func (m *MsgCreateDenomMetadata) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	if deterministic {
		return xxx_messageInfo_MsgCreateDenomMetadata.Marshal(b, m, deterministic)
	} else {
		b = b[:cap(b)]
		n, err := m.MarshalToSizedBuffer(b)
		if err != nil {
			return nil, err
		}
		return b[:n], nil
	}
}
func (m *MsgCreateDenomMetadata) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MsgCreateDenomMetadata.Merge(m, src)
}
func (m *MsgCreateDenomMetadata) XXX_Size() int {
	return m.Size()
}
func (m *MsgCreateDenomMetadata) XXX_DiscardUnknown() {
	xxx_messageInfo_MsgCreateDenomMetadata.DiscardUnknown(m)
}

var xxx_messageInfo_MsgCreateDenomMetadata proto.InternalMessageInfo

func (m *MsgCreateDenomMetadata) GetSenderAddress() string {
	if m != nil {
		return m.SenderAddress
	}
	return ""
}

func (m *MsgCreateDenomMetadata) GetMetadatas() []DenomMetadata {
	if m != nil {
		return m.Metadatas
	}
	return nil
}

// MsgCreateDenomMetadataResponse defines the MsgCreateDenomMetadata response
// type
type MsgCreateDenomMetadataResponse struct {
}

func (m *MsgCreateDenomMetadataResponse) Reset()         { *m = MsgCreateDenomMetadataResponse{} }
func (m *MsgCreateDenomMetadataResponse) String() string { return proto.CompactTextString(m) }
func (*MsgCreateDenomMetadataResponse) ProtoMessage()    {}
func (*MsgCreateDenomMetadataResponse) Descriptor() ([]byte, []int) {
	return fileDescriptor_a5bb4e1c9698e3b7, []int{1}
}
func (m *MsgCreateDenomMetadataResponse) XXX_Unmarshal(b []byte) error {
	return m.Unmarshal(b)
}
func (m *MsgCreateDenomMetadataResponse) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	if deterministic {
		return xxx_messageInfo_MsgCreateDenomMetadataResponse.Marshal(b, m, deterministic)
	} else {
		b = b[:cap(b)]
		n, err := m.MarshalToSizedBuffer(b)
		if err != nil {
			return nil, err
		}
		return b[:n], nil
	}
}
func (m *MsgCreateDenomMetadataResponse) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MsgCreateDenomMetadataResponse.Merge(m, src)
}
func (m *MsgCreateDenomMetadataResponse) XXX_Size() int {
	return m.Size()
}
func (m *MsgCreateDenomMetadataResponse) XXX_DiscardUnknown() {
	xxx_messageInfo_MsgCreateDenomMetadataResponse.DiscardUnknown(m)
}

var xxx_messageInfo_MsgCreateDenomMetadataResponse proto.InternalMessageInfo

// MsgDistributeTokens defines a message that update the denom metadata.
type MsgUpdateDenomMetadata struct {
	// sender_address is the bech32 address of message sender.
	SenderAddress string          `protobuf:"bytes,1,opt,name=sender_address,json=senderAddress,proto3" json:"sender_address,omitempty"`
	Metadatas     []DenomMetadata `protobuf:"bytes,2,rep,name=metadatas,proto3" json:"metadatas"`
}

func (m *MsgUpdateDenomMetadata) Reset()         { *m = MsgUpdateDenomMetadata{} }
func (m *MsgUpdateDenomMetadata) String() string { return proto.CompactTextString(m) }
func (*MsgUpdateDenomMetadata) ProtoMessage()    {}
func (*MsgUpdateDenomMetadata) Descriptor() ([]byte, []int) {
	return fileDescriptor_a5bb4e1c9698e3b7, []int{2}
}
func (m *MsgUpdateDenomMetadata) XXX_Unmarshal(b []byte) error {
	return m.Unmarshal(b)
}
func (m *MsgUpdateDenomMetadata) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	if deterministic {
		return xxx_messageInfo_MsgUpdateDenomMetadata.Marshal(b, m, deterministic)
	} else {
		b = b[:cap(b)]
		n, err := m.MarshalToSizedBuffer(b)
		if err != nil {
			return nil, err
		}
		return b[:n], nil
	}
}
func (m *MsgUpdateDenomMetadata) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MsgUpdateDenomMetadata.Merge(m, src)
}
func (m *MsgUpdateDenomMetadata) XXX_Size() int {
	return m.Size()
}
func (m *MsgUpdateDenomMetadata) XXX_DiscardUnknown() {
	xxx_messageInfo_MsgUpdateDenomMetadata.DiscardUnknown(m)
}

var xxx_messageInfo_MsgUpdateDenomMetadata proto.InternalMessageInfo

func (m *MsgUpdateDenomMetadata) GetSenderAddress() string {
	if m != nil {
		return m.SenderAddress
	}
	return ""
}

func (m *MsgUpdateDenomMetadata) GetMetadatas() []DenomMetadata {
	if m != nil {
		return m.Metadatas
	}
	return nil
}

// MsgUpdateDenomMetadataResponse defines the MsgUpdateDenomMetadata response
// type
type MsgUpdateDenomMetadataResponse struct {
}

func (m *MsgUpdateDenomMetadataResponse) Reset()         { *m = MsgUpdateDenomMetadataResponse{} }
func (m *MsgUpdateDenomMetadataResponse) String() string { return proto.CompactTextString(m) }
func (*MsgUpdateDenomMetadataResponse) ProtoMessage()    {}
func (*MsgUpdateDenomMetadataResponse) Descriptor() ([]byte, []int) {
	return fileDescriptor_a5bb4e1c9698e3b7, []int{3}
}
func (m *MsgUpdateDenomMetadataResponse) XXX_Unmarshal(b []byte) error {
	return m.Unmarshal(b)
}
func (m *MsgUpdateDenomMetadataResponse) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	if deterministic {
		return xxx_messageInfo_MsgUpdateDenomMetadataResponse.Marshal(b, m, deterministic)
	} else {
		b = b[:cap(b)]
		n, err := m.MarshalToSizedBuffer(b)
		if err != nil {
			return nil, err
		}
		return b[:n], nil
	}
}
func (m *MsgUpdateDenomMetadataResponse) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MsgUpdateDenomMetadataResponse.Merge(m, src)
}
func (m *MsgUpdateDenomMetadataResponse) XXX_Size() int {
	return m.Size()
}
func (m *MsgUpdateDenomMetadataResponse) XXX_DiscardUnknown() {
	xxx_messageInfo_MsgUpdateDenomMetadataResponse.DiscardUnknown(m)
}

var xxx_messageInfo_MsgUpdateDenomMetadataResponse proto.InternalMessageInfo

func init() {
	proto.RegisterType((*MsgCreateDenomMetadata)(nil), "rollapp.denommetadata.types.MsgCreateDenomMetadata")
	proto.RegisterType((*MsgCreateDenomMetadataResponse)(nil), "rollapp.denommetadata.types.MsgCreateDenomMetadataResponse")
	proto.RegisterType((*MsgUpdateDenomMetadata)(nil), "rollapp.denommetadata.types.MsgUpdateDenomMetadata")
	proto.RegisterType((*MsgUpdateDenomMetadataResponse)(nil), "rollapp.denommetadata.types.MsgUpdateDenomMetadataResponse")
}

func init() { proto.RegisterFile("denommetadata/tx.proto", fileDescriptor_a5bb4e1c9698e3b7) }

var fileDescriptor_a5bb4e1c9698e3b7 = []byte{
	// 314 bytes of a gzipped FileDescriptorProto
	0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0xff, 0xe2, 0x12, 0x4b, 0x49, 0xcd, 0xcb,
	0xcf, 0xcd, 0x4d, 0x2d, 0x49, 0x4c, 0x49, 0x2c, 0x49, 0xd4, 0x2f, 0xa9, 0xd0, 0x2b, 0x28, 0xca,
	0x2f, 0xc9, 0x17, 0x92, 0x2e, 0xca, 0xcf, 0xc9, 0x49, 0x2c, 0x28, 0xd0, 0x43, 0x91, 0xd7, 0x2b,
	0xa9, 0x2c, 0x48, 0x2d, 0x96, 0x12, 0x49, 0xcf, 0x4f, 0xcf, 0x07, 0xab, 0xd3, 0x07, 0xb1, 0x20,
	0x5a, 0xa4, 0x14, 0x51, 0x8d, 0x42, 0xd5, 0x08, 0x56, 0xa2, 0xd4, 0xcf, 0xc8, 0x25, 0xe6, 0x5b,
	0x9c, 0xee, 0x5c, 0x94, 0x9a, 0x58, 0x92, 0xea, 0x02, 0x52, 0xe0, 0x0b, 0x55, 0x20, 0xa4, 0xca,
	0xc5, 0x57, 0x9c, 0x9a, 0x97, 0x92, 0x5a, 0x14, 0x9f, 0x98, 0x92, 0x52, 0x94, 0x5a, 0x5c, 0x2c,
	0xc1, 0xa8, 0xc0, 0xa8, 0xc1, 0x19, 0xc4, 0x0b, 0x11, 0x75, 0x84, 0x08, 0x0a, 0xf9, 0x71, 0x71,
	0xc2, 0xcc, 0x2c, 0x96, 0x60, 0x52, 0x60, 0xd6, 0xe0, 0x36, 0xd2, 0xd2, 0xc3, 0xe3, 0x56, 0x3d,
	0x14, 0x5b, 0x9c, 0x58, 0x4e, 0xdc, 0x93, 0x67, 0x08, 0x42, 0x18, 0xa1, 0xa4, 0xc0, 0x25, 0x87,
	0xdd, 0x41, 0x41, 0xa9, 0xc5, 0x05, 0xf9, 0x79, 0xc5, 0xa9, 0x30, 0x37, 0x87, 0x16, 0xa4, 0x0c,
	0x2e, 0x37, 0x63, 0x71, 0x10, 0xcc, 0xcd, 0x46, 0x73, 0x99, 0xb8, 0x98, 0x7d, 0x8b, 0xd3, 0x85,
	0x3a, 0x19, 0xb9, 0x84, 0xb1, 0x05, 0xb6, 0x31, 0x5e, 0xeb, 0xb1, 0x07, 0x88, 0x94, 0x35, 0x19,
	0x9a, 0xe0, 0xa1, 0xc8, 0x00, 0x76, 0x0b, 0xb6, 0x40, 0x24, 0xe8, 0x16, 0x2c, 0x9a, 0x08, 0xbb,
	0x05, 0x4f, 0xe8, 0x28, 0x31, 0x38, 0x85, 0x9e, 0x78, 0x24, 0xc7, 0x78, 0xe1, 0x91, 0x1c, 0xe3,
	0x83, 0x47, 0x72, 0x8c, 0x13, 0x1e, 0xcb, 0x31, 0x5c, 0x78, 0x2c, 0xc7, 0x70, 0xe3, 0xb1, 0x1c,
	0x43, 0x94, 0x75, 0x7a, 0x66, 0x49, 0x46, 0x69, 0x92, 0x5e, 0x72, 0x7e, 0xae, 0x7e, 0x4a, 0x65,
	0x6e, 0x6a, 0x5e, 0x71, 0x66, 0x7e, 0x5e, 0x45, 0x65, 0x15, 0x82, 0xa3, 0x5b, 0x94, 0x92, 0xad,
	0x5f, 0xa1, 0x8f, 0x96, 0x6f, 0x40, 0xf6, 0x26, 0xb1, 0x81, 0x53, 0xb9, 0x31, 0x20, 0x00, 0x00,
	0xff, 0xff, 0xae, 0xa3, 0x20, 0x5f, 0x55, 0x03, 0x00, 0x00,
}

// Reference imports to suppress errors if they are not otherwise used.
var _ context.Context
var _ grpc.ClientConn

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
const _ = grpc.SupportPackageIsVersion4

// MsgClient is the client API for Msg service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://godoc.org/google.golang.org/grpc#ClientConn.NewStream.
type MsgClient interface {
	CreateDenomMetadata(ctx context.Context, in *MsgCreateDenomMetadata, opts ...grpc.CallOption) (*MsgCreateDenomMetadataResponse, error)
	UpdateDenomMetadata(ctx context.Context, in *MsgUpdateDenomMetadata, opts ...grpc.CallOption) (*MsgUpdateDenomMetadataResponse, error)
}

type msgClient struct {
	cc grpc1.ClientConn
}

func NewMsgClient(cc grpc1.ClientConn) MsgClient {
	return &msgClient{cc}
}

func (c *msgClient) CreateDenomMetadata(ctx context.Context, in *MsgCreateDenomMetadata, opts ...grpc.CallOption) (*MsgCreateDenomMetadataResponse, error) {
	out := new(MsgCreateDenomMetadataResponse)
	err := c.cc.Invoke(ctx, "/rollapp.denommetadata.types.Msg/CreateDenomMetadata", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *msgClient) UpdateDenomMetadata(ctx context.Context, in *MsgUpdateDenomMetadata, opts ...grpc.CallOption) (*MsgUpdateDenomMetadataResponse, error) {
	out := new(MsgUpdateDenomMetadataResponse)
	err := c.cc.Invoke(ctx, "/rollapp.denommetadata.types.Msg/UpdateDenomMetadata", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// MsgServer is the server API for Msg service.
type MsgServer interface {
	CreateDenomMetadata(context.Context, *MsgCreateDenomMetadata) (*MsgCreateDenomMetadataResponse, error)
	UpdateDenomMetadata(context.Context, *MsgUpdateDenomMetadata) (*MsgUpdateDenomMetadataResponse, error)
}

// UnimplementedMsgServer can be embedded to have forward compatible implementations.
type UnimplementedMsgServer struct {
}

func (*UnimplementedMsgServer) CreateDenomMetadata(ctx context.Context, req *MsgCreateDenomMetadata) (*MsgCreateDenomMetadataResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method CreateDenomMetadata not implemented")
}
func (*UnimplementedMsgServer) UpdateDenomMetadata(ctx context.Context, req *MsgUpdateDenomMetadata) (*MsgUpdateDenomMetadataResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method UpdateDenomMetadata not implemented")
}

func RegisterMsgServer(s grpc1.Server, srv MsgServer) {
	s.RegisterService(&_Msg_serviceDesc, srv)
}

func _Msg_CreateDenomMetadata_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(MsgCreateDenomMetadata)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(MsgServer).CreateDenomMetadata(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/rollapp.denommetadata.types.Msg/CreateDenomMetadata",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(MsgServer).CreateDenomMetadata(ctx, req.(*MsgCreateDenomMetadata))
	}
	return interceptor(ctx, in, info, handler)
}

func _Msg_UpdateDenomMetadata_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(MsgUpdateDenomMetadata)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(MsgServer).UpdateDenomMetadata(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/rollapp.denommetadata.types.Msg/UpdateDenomMetadata",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(MsgServer).UpdateDenomMetadata(ctx, req.(*MsgUpdateDenomMetadata))
	}
	return interceptor(ctx, in, info, handler)
}

var _Msg_serviceDesc = grpc.ServiceDesc{
	ServiceName: "rollapp.denommetadata.types.Msg",
	HandlerType: (*MsgServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "CreateDenomMetadata",
			Handler:    _Msg_CreateDenomMetadata_Handler,
		},
		{
			MethodName: "UpdateDenomMetadata",
			Handler:    _Msg_UpdateDenomMetadata_Handler,
		},
	},
	Streams:  []grpc.StreamDesc{},
	Metadata: "denommetadata/tx.proto",
}

func (m *MsgCreateDenomMetadata) Marshal() (dAtA []byte, err error) {
	size := m.Size()
	dAtA = make([]byte, size)
	n, err := m.MarshalToSizedBuffer(dAtA[:size])
	if err != nil {
		return nil, err
	}
	return dAtA[:n], nil
}

func (m *MsgCreateDenomMetadata) MarshalTo(dAtA []byte) (int, error) {
	size := m.Size()
	return m.MarshalToSizedBuffer(dAtA[:size])
}

func (m *MsgCreateDenomMetadata) MarshalToSizedBuffer(dAtA []byte) (int, error) {
	i := len(dAtA)
	_ = i
	var l int
	_ = l
	if len(m.Metadatas) > 0 {
		for iNdEx := len(m.Metadatas) - 1; iNdEx >= 0; iNdEx-- {
			{
				size, err := m.Metadatas[iNdEx].MarshalToSizedBuffer(dAtA[:i])
				if err != nil {
					return 0, err
				}
				i -= size
				i = encodeVarintTx(dAtA, i, uint64(size))
			}
			i--
			dAtA[i] = 0x12
		}
	}
	if len(m.SenderAddress) > 0 {
		i -= len(m.SenderAddress)
		copy(dAtA[i:], m.SenderAddress)
		i = encodeVarintTx(dAtA, i, uint64(len(m.SenderAddress)))
		i--
		dAtA[i] = 0xa
	}
	return len(dAtA) - i, nil
}

func (m *MsgCreateDenomMetadataResponse) Marshal() (dAtA []byte, err error) {
	size := m.Size()
	dAtA = make([]byte, size)
	n, err := m.MarshalToSizedBuffer(dAtA[:size])
	if err != nil {
		return nil, err
	}
	return dAtA[:n], nil
}

func (m *MsgCreateDenomMetadataResponse) MarshalTo(dAtA []byte) (int, error) {
	size := m.Size()
	return m.MarshalToSizedBuffer(dAtA[:size])
}

func (m *MsgCreateDenomMetadataResponse) MarshalToSizedBuffer(dAtA []byte) (int, error) {
	i := len(dAtA)
	_ = i
	var l int
	_ = l
	return len(dAtA) - i, nil
}

func (m *MsgUpdateDenomMetadata) Marshal() (dAtA []byte, err error) {
	size := m.Size()
	dAtA = make([]byte, size)
	n, err := m.MarshalToSizedBuffer(dAtA[:size])
	if err != nil {
		return nil, err
	}
	return dAtA[:n], nil
}

func (m *MsgUpdateDenomMetadata) MarshalTo(dAtA []byte) (int, error) {
	size := m.Size()
	return m.MarshalToSizedBuffer(dAtA[:size])
}

func (m *MsgUpdateDenomMetadata) MarshalToSizedBuffer(dAtA []byte) (int, error) {
	i := len(dAtA)
	_ = i
	var l int
	_ = l
	if len(m.Metadatas) > 0 {
		for iNdEx := len(m.Metadatas) - 1; iNdEx >= 0; iNdEx-- {
			{
				size, err := m.Metadatas[iNdEx].MarshalToSizedBuffer(dAtA[:i])
				if err != nil {
					return 0, err
				}
				i -= size
				i = encodeVarintTx(dAtA, i, uint64(size))
			}
			i--
			dAtA[i] = 0x12
		}
	}
	if len(m.SenderAddress) > 0 {
		i -= len(m.SenderAddress)
		copy(dAtA[i:], m.SenderAddress)
		i = encodeVarintTx(dAtA, i, uint64(len(m.SenderAddress)))
		i--
		dAtA[i] = 0xa
	}
	return len(dAtA) - i, nil
}

func (m *MsgUpdateDenomMetadataResponse) Marshal() (dAtA []byte, err error) {
	size := m.Size()
	dAtA = make([]byte, size)
	n, err := m.MarshalToSizedBuffer(dAtA[:size])
	if err != nil {
		return nil, err
	}
	return dAtA[:n], nil
}

func (m *MsgUpdateDenomMetadataResponse) MarshalTo(dAtA []byte) (int, error) {
	size := m.Size()
	return m.MarshalToSizedBuffer(dAtA[:size])
}

func (m *MsgUpdateDenomMetadataResponse) MarshalToSizedBuffer(dAtA []byte) (int, error) {
	i := len(dAtA)
	_ = i
	var l int
	_ = l
	return len(dAtA) - i, nil
}

func encodeVarintTx(dAtA []byte, offset int, v uint64) int {
	offset -= sovTx(v)
	base := offset
	for v >= 1<<7 {
		dAtA[offset] = uint8(v&0x7f | 0x80)
		v >>= 7
		offset++
	}
	dAtA[offset] = uint8(v)
	return base
}
func (m *MsgCreateDenomMetadata) Size() (n int) {
	if m == nil {
		return 0
	}
	var l int
	_ = l
	l = len(m.SenderAddress)
	if l > 0 {
		n += 1 + l + sovTx(uint64(l))
	}
	if len(m.Metadatas) > 0 {
		for _, e := range m.Metadatas {
			l = e.Size()
			n += 1 + l + sovTx(uint64(l))
		}
	}
	return n
}

func (m *MsgCreateDenomMetadataResponse) Size() (n int) {
	if m == nil {
		return 0
	}
	var l int
	_ = l
	return n
}

func (m *MsgUpdateDenomMetadata) Size() (n int) {
	if m == nil {
		return 0
	}
	var l int
	_ = l
	l = len(m.SenderAddress)
	if l > 0 {
		n += 1 + l + sovTx(uint64(l))
	}
	if len(m.Metadatas) > 0 {
		for _, e := range m.Metadatas {
			l = e.Size()
			n += 1 + l + sovTx(uint64(l))
		}
	}
	return n
}

func (m *MsgUpdateDenomMetadataResponse) Size() (n int) {
	if m == nil {
		return 0
	}
	var l int
	_ = l
	return n
}

func sovTx(x uint64) (n int) {
	return (math_bits.Len64(x|1) + 6) / 7
}
func sozTx(x uint64) (n int) {
	return sovTx(uint64((x << 1) ^ uint64((int64(x) >> 63))))
}
func (m *MsgCreateDenomMetadata) Unmarshal(dAtA []byte) error {
	l := len(dAtA)
	iNdEx := 0
	for iNdEx < l {
		preIndex := iNdEx
		var wire uint64
		for shift := uint(0); ; shift += 7 {
			if shift >= 64 {
				return ErrIntOverflowTx
			}
			if iNdEx >= l {
				return io.ErrUnexpectedEOF
			}
			b := dAtA[iNdEx]
			iNdEx++
			wire |= uint64(b&0x7F) << shift
			if b < 0x80 {
				break
			}
		}
		fieldNum := int32(wire >> 3)
		wireType := int(wire & 0x7)
		if wireType == 4 {
			return fmt.Errorf("proto: MsgCreateDenomMetadata: wiretype end group for non-group")
		}
		if fieldNum <= 0 {
			return fmt.Errorf("proto: MsgCreateDenomMetadata: illegal tag %d (wire type %d)", fieldNum, wire)
		}
		switch fieldNum {
		case 1:
			if wireType != 2 {
				return fmt.Errorf("proto: wrong wireType = %d for field SenderAddress", wireType)
			}
			var stringLen uint64
			for shift := uint(0); ; shift += 7 {
				if shift >= 64 {
					return ErrIntOverflowTx
				}
				if iNdEx >= l {
					return io.ErrUnexpectedEOF
				}
				b := dAtA[iNdEx]
				iNdEx++
				stringLen |= uint64(b&0x7F) << shift
				if b < 0x80 {
					break
				}
			}
			intStringLen := int(stringLen)
			if intStringLen < 0 {
				return ErrInvalidLengthTx
			}
			postIndex := iNdEx + intStringLen
			if postIndex < 0 {
				return ErrInvalidLengthTx
			}
			if postIndex > l {
				return io.ErrUnexpectedEOF
			}
			m.SenderAddress = string(dAtA[iNdEx:postIndex])
			iNdEx = postIndex
		case 2:
			if wireType != 2 {
				return fmt.Errorf("proto: wrong wireType = %d for field Metadatas", wireType)
			}
			var msglen int
			for shift := uint(0); ; shift += 7 {
				if shift >= 64 {
					return ErrIntOverflowTx
				}
				if iNdEx >= l {
					return io.ErrUnexpectedEOF
				}
				b := dAtA[iNdEx]
				iNdEx++
				msglen |= int(b&0x7F) << shift
				if b < 0x80 {
					break
				}
			}
			if msglen < 0 {
				return ErrInvalidLengthTx
			}
			postIndex := iNdEx + msglen
			if postIndex < 0 {
				return ErrInvalidLengthTx
			}
			if postIndex > l {
				return io.ErrUnexpectedEOF
			}
			m.Metadatas = append(m.Metadatas, DenomMetadata{})
			if err := m.Metadatas[len(m.Metadatas)-1].Unmarshal(dAtA[iNdEx:postIndex]); err != nil {
				return err
			}
			iNdEx = postIndex
		default:
			iNdEx = preIndex
			skippy, err := skipTx(dAtA[iNdEx:])
			if err != nil {
				return err
			}
			if (skippy < 0) || (iNdEx+skippy) < 0 {
				return ErrInvalidLengthTx
			}
			if (iNdEx + skippy) > l {
				return io.ErrUnexpectedEOF
			}
			iNdEx += skippy
		}
	}

	if iNdEx > l {
		return io.ErrUnexpectedEOF
	}
	return nil
}
func (m *MsgCreateDenomMetadataResponse) Unmarshal(dAtA []byte) error {
	l := len(dAtA)
	iNdEx := 0
	for iNdEx < l {
		preIndex := iNdEx
		var wire uint64
		for shift := uint(0); ; shift += 7 {
			if shift >= 64 {
				return ErrIntOverflowTx
			}
			if iNdEx >= l {
				return io.ErrUnexpectedEOF
			}
			b := dAtA[iNdEx]
			iNdEx++
			wire |= uint64(b&0x7F) << shift
			if b < 0x80 {
				break
			}
		}
		fieldNum := int32(wire >> 3)
		wireType := int(wire & 0x7)
		if wireType == 4 {
			return fmt.Errorf("proto: MsgCreateDenomMetadataResponse: wiretype end group for non-group")
		}
		if fieldNum <= 0 {
			return fmt.Errorf("proto: MsgCreateDenomMetadataResponse: illegal tag %d (wire type %d)", fieldNum, wire)
		}
		switch fieldNum {
		default:
			iNdEx = preIndex
			skippy, err := skipTx(dAtA[iNdEx:])
			if err != nil {
				return err
			}
			if (skippy < 0) || (iNdEx+skippy) < 0 {
				return ErrInvalidLengthTx
			}
			if (iNdEx + skippy) > l {
				return io.ErrUnexpectedEOF
			}
			iNdEx += skippy
		}
	}

	if iNdEx > l {
		return io.ErrUnexpectedEOF
	}
	return nil
}
func (m *MsgUpdateDenomMetadata) Unmarshal(dAtA []byte) error {
	l := len(dAtA)
	iNdEx := 0
	for iNdEx < l {
		preIndex := iNdEx
		var wire uint64
		for shift := uint(0); ; shift += 7 {
			if shift >= 64 {
				return ErrIntOverflowTx
			}
			if iNdEx >= l {
				return io.ErrUnexpectedEOF
			}
			b := dAtA[iNdEx]
			iNdEx++
			wire |= uint64(b&0x7F) << shift
			if b < 0x80 {
				break
			}
		}
		fieldNum := int32(wire >> 3)
		wireType := int(wire & 0x7)
		if wireType == 4 {
			return fmt.Errorf("proto: MsgUpdateDenomMetadata: wiretype end group for non-group")
		}
		if fieldNum <= 0 {
			return fmt.Errorf("proto: MsgUpdateDenomMetadata: illegal tag %d (wire type %d)", fieldNum, wire)
		}
		switch fieldNum {
		case 1:
			if wireType != 2 {
				return fmt.Errorf("proto: wrong wireType = %d for field SenderAddress", wireType)
			}
			var stringLen uint64
			for shift := uint(0); ; shift += 7 {
				if shift >= 64 {
					return ErrIntOverflowTx
				}
				if iNdEx >= l {
					return io.ErrUnexpectedEOF
				}
				b := dAtA[iNdEx]
				iNdEx++
				stringLen |= uint64(b&0x7F) << shift
				if b < 0x80 {
					break
				}
			}
			intStringLen := int(stringLen)
			if intStringLen < 0 {
				return ErrInvalidLengthTx
			}
			postIndex := iNdEx + intStringLen
			if postIndex < 0 {
				return ErrInvalidLengthTx
			}
			if postIndex > l {
				return io.ErrUnexpectedEOF
			}
			m.SenderAddress = string(dAtA[iNdEx:postIndex])
			iNdEx = postIndex
		case 2:
			if wireType != 2 {
				return fmt.Errorf("proto: wrong wireType = %d for field Metadatas", wireType)
			}
			var msglen int
			for shift := uint(0); ; shift += 7 {
				if shift >= 64 {
					return ErrIntOverflowTx
				}
				if iNdEx >= l {
					return io.ErrUnexpectedEOF
				}
				b := dAtA[iNdEx]
				iNdEx++
				msglen |= int(b&0x7F) << shift
				if b < 0x80 {
					break
				}
			}
			if msglen < 0 {
				return ErrInvalidLengthTx
			}
			postIndex := iNdEx + msglen
			if postIndex < 0 {
				return ErrInvalidLengthTx
			}
			if postIndex > l {
				return io.ErrUnexpectedEOF
			}
			m.Metadatas = append(m.Metadatas, DenomMetadata{})
			if err := m.Metadatas[len(m.Metadatas)-1].Unmarshal(dAtA[iNdEx:postIndex]); err != nil {
				return err
			}
			iNdEx = postIndex
		default:
			iNdEx = preIndex
			skippy, err := skipTx(dAtA[iNdEx:])
			if err != nil {
				return err
			}
			if (skippy < 0) || (iNdEx+skippy) < 0 {
				return ErrInvalidLengthTx
			}
			if (iNdEx + skippy) > l {
				return io.ErrUnexpectedEOF
			}
			iNdEx += skippy
		}
	}

	if iNdEx > l {
		return io.ErrUnexpectedEOF
	}
	return nil
}
func (m *MsgUpdateDenomMetadataResponse) Unmarshal(dAtA []byte) error {
	l := len(dAtA)
	iNdEx := 0
	for iNdEx < l {
		preIndex := iNdEx
		var wire uint64
		for shift := uint(0); ; shift += 7 {
			if shift >= 64 {
				return ErrIntOverflowTx
			}
			if iNdEx >= l {
				return io.ErrUnexpectedEOF
			}
			b := dAtA[iNdEx]
			iNdEx++
			wire |= uint64(b&0x7F) << shift
			if b < 0x80 {
				break
			}
		}
		fieldNum := int32(wire >> 3)
		wireType := int(wire & 0x7)
		if wireType == 4 {
			return fmt.Errorf("proto: MsgUpdateDenomMetadataResponse: wiretype end group for non-group")
		}
		if fieldNum <= 0 {
			return fmt.Errorf("proto: MsgUpdateDenomMetadataResponse: illegal tag %d (wire type %d)", fieldNum, wire)
		}
		switch fieldNum {
		default:
			iNdEx = preIndex
			skippy, err := skipTx(dAtA[iNdEx:])
			if err != nil {
				return err
			}
			if (skippy < 0) || (iNdEx+skippy) < 0 {
				return ErrInvalidLengthTx
			}
			if (iNdEx + skippy) > l {
				return io.ErrUnexpectedEOF
			}
			iNdEx += skippy
		}
	}

	if iNdEx > l {
		return io.ErrUnexpectedEOF
	}
	return nil
}
func skipTx(dAtA []byte) (n int, err error) {
	l := len(dAtA)
	iNdEx := 0
	depth := 0
	for iNdEx < l {
		var wire uint64
		for shift := uint(0); ; shift += 7 {
			if shift >= 64 {
				return 0, ErrIntOverflowTx
			}
			if iNdEx >= l {
				return 0, io.ErrUnexpectedEOF
			}
			b := dAtA[iNdEx]
			iNdEx++
			wire |= (uint64(b) & 0x7F) << shift
			if b < 0x80 {
				break
			}
		}
		wireType := int(wire & 0x7)
		switch wireType {
		case 0:
			for shift := uint(0); ; shift += 7 {
				if shift >= 64 {
					return 0, ErrIntOverflowTx
				}
				if iNdEx >= l {
					return 0, io.ErrUnexpectedEOF
				}
				iNdEx++
				if dAtA[iNdEx-1] < 0x80 {
					break
				}
			}
		case 1:
			iNdEx += 8
		case 2:
			var length int
			for shift := uint(0); ; shift += 7 {
				if shift >= 64 {
					return 0, ErrIntOverflowTx
				}
				if iNdEx >= l {
					return 0, io.ErrUnexpectedEOF
				}
				b := dAtA[iNdEx]
				iNdEx++
				length |= (int(b) & 0x7F) << shift
				if b < 0x80 {
					break
				}
			}
			if length < 0 {
				return 0, ErrInvalidLengthTx
			}
			iNdEx += length
		case 3:
			depth++
		case 4:
			if depth == 0 {
				return 0, ErrUnexpectedEndOfGroupTx
			}
			depth--
		case 5:
			iNdEx += 4
		default:
			return 0, fmt.Errorf("proto: illegal wireType %d", wireType)
		}
		if iNdEx < 0 {
			return 0, ErrInvalidLengthTx
		}
		if depth == 0 {
			return iNdEx, nil
		}
	}
	return 0, io.ErrUnexpectedEOF
}

var (
	ErrInvalidLengthTx        = fmt.Errorf("proto: negative length found during unmarshaling")
	ErrIntOverflowTx          = fmt.Errorf("proto: integer overflow")
	ErrUnexpectedEndOfGroupTx = fmt.Errorf("proto: unexpected end of group")
)