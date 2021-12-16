from __future__ import annotations

from io import BytesIO
from typing import BinaryIO


def binsum(digits: list[int]) -> int:
    total = 0
    for digit in digits:
        total = total * 2 + digit
    return total


def binsplit(number: int, pad: int | None = None) -> list[int]:
    digits: list[int] = list()
    while number:
        digits.insert(0, number % 2)
        number //= 2
    if pad is not None:
        while len(digits) < pad:
            digits.insert(0, 0)
    return digits


class BinReader:
    def __init__(self, file: BinaryIO) -> None:
        pass

    def read(self, size: int) -> int:
        raise NotImplementedError

    def tell(self) -> int:
        raise NotImplementedError


class BinReader1(BinReader):
    """Doesn't work, not sure why"""

    file: BinaryIO
    charindex: int = 0
    char: list[int]

    def __init__(self, file: BinaryIO) -> None:
        self.file = file
        self.charindex = 0
        self.char = []

    def read(self, size: int) -> int:
        if size == 0:
            return 0
        elif size < 0:
            raise NotImplementedError

        until = self.charindex + size
        bytecount, lastcharindex = until // 8, until % 8

        if not self.char:
            total = 0
        elif not bytecount:
            total = binsum(self.char[self.charindex : lastcharindex])
            self.charindex = lastcharindex
            return total
        else:
            bytecount -= 1
            total = binsum(self.char[self.charindex :])

        readbytes = self.file.read(bytecount)

        for byte in readbytes:
            total = (total << 8) + byte

        if lastcharindex:
            self.charindex = lastcharindex
            self.char = binsplit(self.file.read(1)[0], 8)
            total = (total << lastcharindex) + binsum(self.char[:lastcharindex])
        else:
            self.charindex = 0
            self.char = []

        return total

    def tell(self) -> int:
        return self.file.tell() * 8 - 8 + self.charindex


class BinReader2(BinReader):
    file: BinaryIO
    char: list[int]

    def __init__(self, file: BinaryIO) -> None:
        self.file = file
        self.char = []

    def read(self, size: int) -> int:
        if size == 0:
            return 0
        elif size < 0:
            raise NotImplementedError

        while len(self.char) < size:
            self.char.extend(binsplit(self.file.read(1)[0], 8))

        ret = self.char[:size]
        self.char = self.char[size:]
        return binsum(ret)

    def tell(self) -> int:
        return self.file.tell() * 8 - len(self.char)


class Packet:
    PACKET_TYPES: dict[int, type[Packet]] = {}

    version: int
    type_id: int

    def __init__(self, version: int, type_id: int, b: BinReader):
        self.version = version
        self.type_id = type_id

    def __init_subclass__(cls, *, packet_type_id: int | None = None) -> None:
        if packet_type_id is not None:
            cls.PACKET_TYPES[packet_type_id] = cls

    @classmethod
    def parse(cls, b: BinReader) -> Packet:
        version = b.read(3)
        type_id = b.read(3)
        return cls.PACKET_TYPES[type_id](version=version, type_id=type_id, b=b)

    def calculate_value(self) -> int:
        raise NotImplementedError


class PacketLiteral(Packet, packet_type_id=4):
    value: int

    def __init__(self, version: int, type_id: int, b: BinReader):
        super().__init__(version, type_id, b)

        value = 0
        continuebit = 1
        while continuebit > 0:
            continuebit = b.read(1)
            value = (value << 4) + b.read(4)
        self.value = value

    def __repr__(self) -> str:
        return f"<{type(self).__name__} {self.value}>"

    def calculate_value(self) -> int:
        return self.value


class PacketOperator(Packet):
    packets: list[Packet]

    def __init__(self, version: int, type_id: int, b: BinReader):
        super().__init__(version, type_id, b)

        packets: list[Packet] = []
        self.packets = packets

        length_type_id = b.read(1)
        if length_type_id == 1:
            num_sub = b.read(11)
            for _ in range(num_sub):
                packets.append(Packet.parse(b))
        else:
            total_len = b.read(15)
            end = b.tell() + total_len
            while b.tell() < end:
                packets.append(Packet.parse(b))

        self.packets = packets

    def __repr__(self) -> str:
        return f"<{type(self).__name__} {self.packets}>"


# for i in range(8):
#     if i == 4:
#         continue

#     class StubPacketOperator(PacketOperator, packet_type_id=i):
#         pass


class PacketOperatorSum(PacketOperator, packet_type_id=0):
    def calculate_value(self) -> int:
        return sum(p.calculate_value() for p in self.packets)


class PacketOperatorProduct(PacketOperator, packet_type_id=1):
    def calculate_value(self) -> int:
        i = 1
        for p in self.packets:
            i *= p.calculate_value()
        return i


class PacketOperatorMinimum(PacketOperator, packet_type_id=2):
    def calculate_value(self) -> int:
        return min(p.calculate_value() for p in self.packets)


class PacketOperatorMaximum(PacketOperator, packet_type_id=3):
    def calculate_value(self) -> int:
        return max(p.calculate_value() for p in self.packets)


class PacketOperatorGreaterThan(PacketOperator, packet_type_id=5):
    def calculate_value(self) -> int:
        return (
            1
            if self.packets[0].calculate_value() > self.packets[1].calculate_value()
            else 0
        )


class PacketOperatorLessThan(PacketOperator, packet_type_id=6):
    def calculate_value(self) -> int:
        return (
            1
            if self.packets[0].calculate_value() < self.packets[1].calculate_value()
            else 0
        )


class PacketOperatorEqualTo(PacketOperator, packet_type_id=7):
    def calculate_value(self) -> int:
        return (
            1
            if self.packets[0].calculate_value() == self.packets[1].calculate_value()
            else 0
        )


def sum_versions(packet: Packet) -> int:
    total = packet.version
    if isinstance(packet, PacketOperator):
        for subpacket in packet.packets:
            total += sum_versions(subpacket)
    return total


if __name__ == "__main__":
    PUZZLE_INPUT = bytes.fromhex(input())
    # b = BinReader1(BytesIO(PUZZLE_INPUT))
    b = BinReader2(BytesIO(PUZZLE_INPUT))
    p = Packet.parse(b)
    print("Part 1:", sum_versions(p))
    print("Part 2:", p.calculate_value())
