#include "pch.h"
#include "framework.h"
#include "Charactor.hpp"
#include "String.hpp" // needed for Charactor::ToString()
#include <cstddef>

namespace Puma {
namespace Types
{
	namespace
	{
		constexpr std::uint8_t UTF8CharSizeLookup[32] =
		{
			1, 1, 1, 1, 1, 1, 1, 1,
			1, 1, 1, 1, 1, 1, 1, 1,
			1, 1, 1, 1, 1, 1, 1, 1,
			2, 2, 2, 2,
			3, 3,
			4,
			1
		};
	}

	Charactor::Charactor() noexcept
		: packedValue(0U)
	{
	}

	Charactor::Charactor(const Charactor& source) noexcept
		: packedValue(source.packedValue)
	{
	}

	Charactor::Charactor(const char* data, std::size_t size) noexcept
		: Charactor(reinterpret_cast<const std::uint8_t*>(data), static_cast<std::uint32_t>(size))
	{
	}

	Charactor::Charactor(const std::uint8_t* data, std::uint32_t dataSize) noexcept
	: packedValue(0U)
	{
		if (data == nullptr || dataSize == 0)
		{
			return;
		}

		const std::uint8_t first = static_cast<std::uint8_t>(data[0]);
		const std::uint8_t size = GetCharSize(first); // 1..4

		if (size > 4 || size > dataSize)
		{
			// Invalid size, treat as empty character.
			return;
		}

		// Copy up to 4 bytes
		memcpy_s(codeUnits, sizeof(codeUnits), data, size);
	}

	Charactor::~Charactor() noexcept = default;

	Charactor& Charactor::operator=(const Charactor& source) noexcept
	{
		if (this != &source)
		{
			packedValue = source.packedValue;
		}
		return *this;
	}

	Types::String Charactor::ToString() const noexcept
	{
		std::uint8_t buffer[4] = { 0 };

		const std::uint8_t first = codeUnits[0];
		if (first == 0U)
		{
			// Empty character -> empty String
			return Types::String();
		}

		const std::uint8_t size = GetCharSize(first); // 1..4
		// Copy up to 4 bytes
		std::memcpy(buffer, codeUnits, size);

		return String(buffer, size);
	}

	std::uint8_t Charactor::GetCharSize(std::uint8_t c) noexcept
	{
		return UTF8CharSizeLookup[c >> 3];
	}
} // namespace Types
} // namespace Puma