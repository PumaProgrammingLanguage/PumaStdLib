#pragma once

#include <cstdint>

namespace Puma {
namespace Types
{
	// Forward declaration to avoid circular include with String.hpp
	union String;

	// Represents a UTF-8 character (code point) as a sequence of up to 4 bytes.
	union Charactor
	{
	public:
		// Lifetime
		Charactor() noexcept;
		Charactor(const Charactor& source) noexcept;
		Charactor(const uint8_t* data) noexcept;
		~Charactor() noexcept;

		// Assignment
		Charactor& operator=(const Charactor& source) noexcept;

		// Convert this UTF-8 character into a Puma String.
		String ToString() const noexcept;

		// Get pointer to the UTF-8 code unit.
		uint8_t* ToUTF8() const noexcept;

		// Returns the number of bytes in the UTF‑8 code unit sequence starting with 'c'.
		// Invalid leading bytes and continuation bytes return 1.
		static uint8_t GetCharSize(const uint8_t c) noexcept;
		uint8_t GetCharSize() noexcept;

	private:
		// Raw 4-byte representation (e.g., UTF-8 bytes)
		uint8_t codeUnits[4];
		// Packed 32-bit representation of the same 4 bytes
		uint32_t packedValue;
	};
} // namespace Types
} // namespace Puma